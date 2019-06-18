-module(lp5562).

%% API exports
-export([start_link/0,
         start_link/2,
         set_color/2,
         blink/2]).

%% internal exports
-export([init_device/1,
         reset_device/1,
         load_engine/2,
         set_current/3,
         set_brightness/3,
         set_engine_mode/2,
         write_engine/3,
         run_engines/2,
         read_reg/2,
         write_reg/3,
         compile/1,
         encode/1]).

-type engine() :: 1 | 2 | 3.
-type cmd() ::
        start
      | fin
      | {fin, int | reset}
      | {fin, Int::0|1, Reset::0|1}
      | {label, Label::atom()}
      | {branch, Count::0..63, Label::atom()}
      | {branch, Count::0..63, Index::non_neg_integer()}
      | {wait, TimeMS::float()}
      | {set_pwm, Value::0..255}
      | {ramp, TimeMS::float(), Steps::integer()}
      | {ramp, Prescale::0|1, StepTime::non_neg_integer(), Increments::integer()}
      | {trigger, Sends::[engine()], Waits::[engine()]}.

-type cmd_bin() :: <<_:16>>.

-define(PROGRAM_LENGTH, 32).

%% ENABLE Register 00h
-define(REG_ENABLE,  16#00).
%% it takes more 488 us to update ENABLE register
-define(WAIT_ENABLE_DONE, timer:sleep(1)).
-define(EXEC_ENG1_M, 16#30).
-define(EXEC_ENG2_M, 16#0C).
-define(EXEC_ENG3_M, 16#03).
-define(EXEC_M     , 16#3F).
-define(MASTER_ENABLE,   16#40).
-define(LOGARITHMIC_PWM, 16#80).
-define(ENABLE_DEFAULT,	(?MASTER_ENABLE bor ?LOGARITHMIC_PWM)).

%% OPMODE Register 01h
-define(REG_OP_MODE, 16#01).
%% operation mode change needs to be longer than 153 us
-define(WAIT_OP_MODE_DONE, timer:sleep(1)).
-define(MODE_ENG1_M, 16#30).
-define(MODE_ENG2_M, 16#0C).
-define(MODE_ENG3_M, 16#03).
-define(LOAD_ENG1,   16#10).
-define(LOAD_ENG2,   16#04).
-define(LOAD_ENG3,   16#01).
-define(LOAD_ENG_M,  16#15).
-define(RUN_ENG1,    16#20).
-define(RUN_ENG2,    16#08).
-define(RUN_ENG3,    16#02).

%% CONFIG Register 08h
-define(REG_CONFIG, 16#08).
-define(PWM_HF,     16#40).
-define(PWRSAVE_EN, 16#20).
-define(CLK_INT,    16#01). %% Internal clock
-define(DEFAULT_CFG, ?CLK_INT bor ?PWM_HF).

%% BRIGHTNESS Registers
-define(REG_R_PWM, 16#04).
-define(REG_G_PWM, 16#03).
-define(REG_B_PWM, 16#02).
-define(REG_W_PWM, 16#0E).

%% CURRENT Registers
-define(REG_R_CURRENT, 16#07).
-define(REG_G_CURRENT, 16#06).
-define(REG_B_CURRENT, 16#05).
-define(REG_W_CURRENT, 16#0F).

%% LEDMAP Register 70h
-define(REG_ENG_SEL,   16#70).
-define(ENG_SEL_PWM,   16#00).
-define(ENG_FOR_RGB_M, 16#3F).
-define(ENG_SEL_RGB,   16#1B). % R:ENG1, G:ENG2, B:ENG3
-define(ENG_FOR_W_M,   16#C0).
-define(ENG1_FOR_W,    16#40). % W:ENG1
-define(ENG2_FOR_W,    16#80). % W:ENG2
-define(ENG3_FOR_W,    16#C0). % W:ENG3

%% PROGRAM ENGINE Registers
-define(REG_PROG_MEM_ENG1, 16#10).
-define(REG_PROG_MEM_ENG2, 16#30).
-define(REG_PROG_MEM_ENG3, 16#50).

%% Program Commands
-define(CMD_DISABLE, 16#00).
-define(CMD_LOAD   , 16#15).
-define(CMD_RUN	   , 16#2A).
-define(CMD_DIRECT , 16#3F).
-define(PATTERN_OFF, 16#00).

%% RESET Register 0Dh
-define(REG_RESET, 16#0D).
-define(RESET	 , 16#FF).


start_link() ->
    start_link("i2c-1", 16330).

start_link(Dev, Addr) ->
    {ok, Pid} = i2c:start_link(Dev, Addr),
    init_device(Pid),
    {ok, Pid}.


-spec program_engines(pid(), [{engine(), [cmd()]}]) -> ok.
program_engines(Ctrl, Progs) ->
    lists:foreach(fun({Engine, Prog}) ->
                          Bin = compile(Prog),
                          load_engine(Ctrl, Engine),
                          write_engine(Ctrl, Engine, Bin)
                  end, Progs).

set_color(Ctrl, {R, G, B}) ->
    MkProg = fun(Color) ->
                  [{set_pwm, Color}, fin]
             end,
    Progs = [
             {1,  MkProg(R)},
             {2,  MkProg(G)},
             {3,  MkProg(B)}
            ],
    program_engines(Ctrl, Progs),
    run_engines(Ctrl, [1, 2, 3]).


blink(Ctrl, {R, G, B}) ->
    MkProg = fun(Color) ->
                     [{set_pwm, 0},
                      {label, blink},
                      {ramp, 300, Color},
                      {ramp, 300, -Color},
                      {branch, 0, blink}
                     ]
             end,
    Progs = [
             {1, MkProg(R)},
             {2, MkProg(G)},
             {3, MkProg(B)}
             ],
    program_engines(Ctrl, Progs),
    run_engines(Ctrl, [1, 2, 3]).


write_reg(Ctrl, Reg, Val) ->
    i2c:write(Ctrl, <<Reg:8, Val:8/unsigned>>).

read_reg(Ctrl, Reg) ->
    <<Val:8/unsigned>> = i2c:write_read(Ctrl, <<Reg:8>>, 1),
    Val.

update_reg_bits(Ctrl, Reg, Mask, Val) ->
    Current = read_reg(Ctrl, Reg),
    Tmp = Current band (bnot Mask),
    NewVal = Tmp bor (Val band Mask),
    write_reg(Ctrl, Reg, NewVal).

init_device(Ctrl) ->
    write_reg(Ctrl, ?REG_ENABLE, ?ENABLE_DEFAULT),
    ?WAIT_ENABLE_DONE,
    write_reg(Ctrl, ?REG_OP_MODE, ?CMD_DIRECT),
    ?WAIT_OP_MODE_DONE,
    write_reg(Ctrl, ?REG_CONFIG, ?DEFAULT_CFG),
    [set_current(Ctrl, L, 100) || L <- [red, green, blue, white]],
    [set_brightness(Ctrl, L, 0) || L <- [red, green, blue, white]],
    %% Set LED map as register PWM by default
    write_reg(Ctrl, ?REG_OP_MODE, ?ENG_SEL_PWM).

reset_device(Ctrl) ->
    write_reg(Ctrl, ?REG_RESET, ?RESET).

set_current(Ctrl, red, Val) ->
    write_reg(Ctrl, ?REG_R_CURRENT, Val);
set_current(Ctrl, green, Val) ->
    write_reg(Ctrl, ?REG_G_CURRENT, Val);
set_current(Ctrl, blue, Val) ->
    write_reg(Ctrl, ?REG_B_CURRENT, Val);
set_current(Ctrl, white, Val) ->
    write_reg(Ctrl, ?REG_W_CURRENT, Val).

set_brightness(Ctrl, red, Val) ->
    write_reg(Ctrl, ?REG_R_PWM, Val);
set_brightness(Ctrl, green, Val) ->
    write_reg(Ctrl, ?REG_G_PWM, Val);
set_brightness(Ctrl, blue, Val) ->
    write_reg(Ctrl, ?REG_B_PWM, Val);
set_brightness(Ctrl, white, Val) ->
    write_reg(Ctrl, ?REG_W_PWM, Val).


load_engine(Ctrl, Engine) ->
    Mode = read_reg(Ctrl, ?REG_OP_MODE),
    LoadBits = case Engine of
                   1 -> ?LOAD_ENG1;
                   2 -> ?LOAD_ENG2;
                   3 -> ?LOAD_ENG3
              end,
    write_reg(Ctrl, ?REG_OP_MODE, Mode bor LoadBits),
    ?WAIT_OP_MODE_DONE.

set_engine_mode(Ctrl, rgb) ->
    update_reg_bits(Ctrl, ?REG_ENG_SEL, ?ENG_FOR_RGB_M, ?ENG_SEL_RGB);
set_engine_mode(Ctrl, {white, Engine}) ->
    Val = case Engine of
              1 -> ?ENG1_FOR_W;
              2 -> ?ENG2_FOR_W;
              3 -> ?ENG3_FOR_W
          end,
    update_reg_bits(Ctrl, ?REG_ENG_SEL, ?ENG_FOR_W_M, Val).

write_engine(Ctrl, Engine, Bin) ->
    Base = case Engine of
               1 -> ?REG_PROG_MEM_ENG1;
               2 -> ?REG_PROG_MEM_ENG2;
               3 -> ?REG_PROG_MEM_ENG3
           end,
    %% Clear out space
    lists:foreach(fun(Offset) ->
                          i2c:write(Ctrl, <<(Base+Offset):8, 16#00>>)
                  end, lists:seq(1, 32)),
    i2c:write(Ctrl, <<Base:8, Bin/binary>>).

run_engines(Ctrl, Engines) ->
    Mode = read_reg(Ctrl, ?REG_OP_MODE),
    Exec = read_reg(Ctrl, ?REG_ENABLE),

    MaskVal = fun(Engine) ->
                      case Engine of
                          1 -> {?MODE_ENG1_M, ?EXEC_ENG1_M, ?RUN_ENG1};
                          2 -> {?MODE_ENG2_M, ?EXEC_ENG2_M, ?RUN_ENG2};
                          3 -> {?MODE_ENG3_M, ?EXEC_ENG3_M, ?RUN_ENG3}
                      end
              end,
    {NewMode, NewExec} = lists:foldl(fun(Engine, {M, E}) ->
                                             {ModeMask, ExecMask, Val} = MaskVal(Engine),
                                             {(M band bnot ModeMask) bor Val,
                                              (E band bnot ExecMask) bor Val}
                                     end, {Mode, Exec}, Engines),
    write_reg(Ctrl, ?REG_OP_MODE, NewMode),
    ?WAIT_OP_MODE_DONE,
    update_reg_bits(Ctrl, ?REG_ENABLE, ?EXEC_M, NewExec),
    ?WAIT_ENABLE_DONE.


%%
%% Language
%%

-spec compile([cmd()]) -> binary().
compile(Program) ->
    list_to_binary(compile(Program, #{}, 0, [])).

-spec compile([cmd()], Labels::map(), CmdIndex::non_neg_integer(), Acc::[cmd_bin()]) -> [cmd_bin()].
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


-spec encode(cmd()) -> cmd_bin().
encode(start) ->
    <<0:16>>;
encode(fin) ->
    encode({fin, 0, 0});
encode({fin, Arg})  ->
    Check = fun(V) when Arg == V  ->
                    1;
               (_) ->
                    0
            end,
    encode({fin, Check(int), Check(reset)});
encode({fin, Int, Reset}) when is_integer(Int), is_integer(Reset) ->
    <<1:1, 1:1, 0:1, Int:1, Reset:1, 1:11>>;
encode({branch, Count, StepNumber}) when is_integer(StepNumber) ->
    <<1:1, 0:1, 1:1, Count:6, 0:3, StepNumber:4>>;
encode({wait, Time}) ->
    encode({ramp, Time, 0});
encode({ramp, Time, Steps}) when Time >= 0, Time =< 31 ->
    encode({ramp, 1, round(Time / 0.492), Steps});
encode({ramp, Time, Steps}) when Time > 31, Time =<1000 ->
    encode({ramp, 0, round(Time / 15.87), Steps});
encode({ramp, Prescale, StepTime, Steps})  ->
    Sign = case Steps < 0 of
               true -> 1;
               _ -> -0
           end,
    <<0:1, Prescale:1, StepTime:6, Sign:1, (abs(Steps) - 1):7>>;
encode({set_pwm, PWM}) ->
    <<2#01000000:8, PWM:8>>;
encode({trigger, Sends, Waits}) ->
    Convert = fun(Engines) ->
                      lists:foldl(fun(Engine, Acc) ->
                                          Acc bor (1 bsl Engine)
                                  end, 0, Engines)
              end,
        <<1:1, 1:1, 1:1, (Convert(Waits)):3, 0:1, 0:1, 0:1, (Convert(Sends)):3, 0:1>>.



%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
%% main(Args) ->
%%     Bin = lp5562_lang:compile([
%%                                {ramp, 50, 100},
%%                                {ramp, 50, -100},
%%                                {wait, 10}
%%                               ]),

%%     {ok, Ctrl} = i2c:start_link("i2c-1", 16#30),
%%     lp5562:init_device(Ctrl),
%%     lp5562:set_engine_mode(Ctrl, rgb),
%%     lp5562:load_engine(Ctrl, 2),
%%     lp5562:write_engine(Ctrl, 2, Bin),
%%     lp5562:run_engines(Ctrl, [2]),

%%     ok.

%%====================================================================
%% Internal functions
%%====================================================================
