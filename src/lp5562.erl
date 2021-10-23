-module(lp5562).

%% API exports
-export([
    init/0,
    init/1,
    set_color/2,
    blink/2, blink/3
]).

%% internal exports
-export([
    load_engine/2,
    select_engine/2,
    write_engine/2,
    run_engines/1,
    to_hex/1,
    compile/1,
    encode/1
]).

-type engine() :: 1 | 2 | 3.
-type cmd() ::
    start
    | fin
    | {fin, Int :: 0 | 1, Reset :: 0 | 1}
    | {label, Label :: atom()}
    | {branch, Count :: 0..63, Label :: atom()}
    | {branch, Count :: 0..63, Index :: non_neg_integer()}
    | {wait, TimeMS :: float()}
    | {set_pwm, Value :: 0..255}
    | {ramp, TimeMS :: float(), Steps :: integer()}
    | {ramp, Prescale :: 0 | 1, StepTime :: non_neg_integer(), Increments :: integer()}
    | {trigger, Sends :: [engine()], Waits :: [engine()]}.

-type cmd_bin() :: <<_:16>>.

-define(DEFAULT_PATH_DEVICE, "/sys/bus/i2c/devices/1-0030").
-define(PATH_DEVICE(B, X), B#state.device_base ++ "/" ++ X).

-define(PATH_FIRMWARE, "/sys/class/firmware/lp5562").
-define(PATH_LOADING, ?PATH_FIRMWARE ++ "/loading").
-define(PATH_DATA, ?PATH_FIRMWARE ++ "/data").

-record(state, {
    device_base :: string()
}).

-type state() :: #state{}.

-export_type([state/0]).

%% echo 1 > /sys/bus/i2c/devices/1-0030/select_engine
%% echo "RGB" > /sys/bus/i2c/devices/1-0030/engine_mux
%% echo 1 > /sys/class/firmware/lp5562/loading
%% echo "4000136313E3A001" > /sys/class/firmware/lp5562/data
%% echo 0 >  /sys/class/firmware/lp5562/loading
%% echo 1 > /sys/bus/i2c/devices/1-0030/run_engine

init() ->
    init("/sys/bus/i2c/devices/1-0030").

init(LedPath) ->
    State = #state{device_base = LedPath},
    write("RGB", ?PATH_DEVICE(State, "engine_mux")),
    {ok, State}.

-spec write(Value :: string(), Path :: string()) -> ok | {error, term()}.
write(Str, Path) ->
    case file:open(Path, [raw, write, binary]) of
        {ok, Fd} ->
            case file:write(Fd, Str) of
                ok -> file:close(Fd);
                {error, Reason} -> {error, Reason}
            end;
        {error, Error} ->
            {error, Error}
    end.

-spec program_engines([{engine(), [cmd()]}], #state{}) -> ok.
program_engines(Progs, State) ->
    lists:foreach(
        fun({Engine, Prog}) ->
            ProgStr = to_hex(Prog),
            select_engine(Engine, State),
            load_engine(true, State),
            write_engine(ProgStr, State),
            load_engine(false, State)
        end,
        Progs
    ).

set_color({R, G, B}, State) ->
    MkProg = fun(Color) ->
        [{set_pwm, Color}]
    end,
    Progs = [
        {1, MkProg(R)},
        {2, MkProg(G)},
        {3, MkProg(B)}
    ],
    program_engines(Progs, State),
    run_engines(State).

blink({R, G, B}, State) ->
    blink({R, G, B}, 250, State).

blink({R, G, B}, HalfTime, State) ->
    MkProg = fun(Color) ->
        case Color of
            0 ->
                [{set_pwm, 0}];
            _ ->
                [
                    {set_pwm, 0},
                    {label, blink},
                    {ramp, HalfTime, 255},
                    {ramp, HalfTime, -255},
                    {branch, 0, blink}
                ]
        end
    end,
    Progs = [
        {1, MkProg(R)},
        {2, MkProg(G)},
        {3, MkProg(B)}
    ],
    program_engines(Progs, State),
    run_engines(State).

select_engine(Engine, State) ->
    EngineStr = integer_to_list(Engine),
    Result = write(EngineStr, ?PATH_DEVICE(State, "select_engine")),
    timer:sleep(100),
    Result.

load_engine(Load, _State) ->
    Value =
        case Load of
            true -> "1";
            _ -> "0"
        end,
    Result = write(Value, ?PATH_LOADING),
    timer:sleep(100),
    Result.

write_engine(ProgStr, _State) ->
    write(ProgStr, ?PATH_DATA).

run_engines(State) ->
    write("1", ?PATH_DEVICE(State, "run_engine")).

%%
%% Language
%%

-spec compile([cmd()]) -> binary().
compile(Program) ->
    list_to_binary(compile(Program, #{}, 0, [])).

-spec compile([cmd()], Labels :: map(), CmdIndex :: non_neg_integer(), Acc :: [cmd_bin()]) ->
    [cmd_bin()].
compile([], _, _, Acc) ->
    lists:reverse(Acc);
compile([{label, Name} | Tail], Labels, Index, Acc) ->
    compile(Tail, maps:put(Name, Index, Labels), Index + 1, Acc);
compile([{branch, Count, Label} | Tail], Labels, Index, Acc) when is_atom(Label) ->
    case maps:get(Label, Labels, false) of
        false ->
            error({unkown_label, {Label, Index}});
        StepNumber ->
            compile([{branch, Count, StepNumber} | Tail], Labels, Index, Acc)
    end;
compile([Cmd | Tail], Labels, Index, Acc) ->
    compile(Tail, Labels, Index + 1, [encode(Cmd) | Acc]).

to_hex(Bin) when is_binary(Bin) ->
    [Y || <<X:4>> <= Bin, Y <- integer_to_list(X, 16)];
to_hex(Cmds) when is_list(Cmds) ->
    to_hex(compile(Cmds)).

-spec encode(cmd()) -> cmd_bin().
encode(start) ->
    <<0:16>>;
encode(fin) ->
    encode({fin, 0, 0});
encode({fin, Int, Reset}) when is_integer(Int), is_integer(Reset) ->
    <<1:1, 1:1, 0:1, Int:1, Reset:1, 1:11>>;
encode({branch, Count, StepNumber}) when is_integer(StepNumber) ->
    <<1:1, 0:1, 1:1, Count:6, 0:3, StepNumber:4>>;
encode({wait, Time}) ->
    encode({ramp, Time, 0});
encode({ramp, Time, Steps}) when Time >= 0, Time =< 31 ->
    encode({ramp, 1, round(Time / 0.492), Steps});
encode({ramp, Time, Steps}) when Time > 31, Time =< 1000 ->
    encode({ramp, 0, round(Time / 15.87), Steps});
encode({ramp, Prescale, StepTime, Steps}) ->
    Sign =
        case Steps < 0 of
            true -> 1;
            _ -> -0
        end,
    <<0:1, Prescale:1, StepTime:6, Sign:1, (abs(Steps) - 1):7>>;
encode({set_pwm, PWM}) ->
    <<2#01000000:8, PWM:8>>;
encode({trigger, Sends, Waits}) ->
    Convert = fun(Engines) ->
        lists:foldl(
            fun(Engine, Acc) ->
                Acc bor (1 bsl Engine)
            end,
            0,
            Engines
        )
    end,
    <<1:1, 1:1, 1:1, (Convert(Waits)):3, 0:1, 0:1, 0:1, (Convert(Sends)):3, 0:1>>.
