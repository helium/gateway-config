-module(gateway_config_led).

-include_lib("gatt/include/gatt.hrl").
-include_lib("ebus/include/ebus.hrl").

-include("gateway_config.hrl").

-define(BLUEZ_OBJECT_PATH, "/org/bluez/hci0").
-define(BLUEZ_MEMBER_PROPERTIES_CHANGED, "PropertiesChanged").

-define(COLOR_OFF, {0, 0, 0}).
-define(COLOR_RED, {255, 0, 0}).
-define(COLOR_GREEN, {0, 255, 0}).
-define(COLOR_BLUE, {0, 0, 255}).
-define(COLOR_ORANGE, {255, 255, 0}).

-define(DIALABLE_TIMEOUT, 60 * 1000).
%% How often to go fetch diagnostics.
-define(DIAGNOSTICS_TIMEOUT, 30 * 1000).

%% API
-export([
    lights_event/1,
    lights_info/0
]).

%% gen_server
-export([
    start_link/1,
    init/1,
    handle_info/2,
    handle_call/3,
    handle_cast/2,
    terminate/2
]).

-type led_event() :: panic | disable | enable | start_advert | stop_advert | online | offline.
-type led_state() :: undefined | panic | disable | {advert, term()} | online | offline.

-record(state, {
    handle = undefined :: lp5562:state() | undefined,
    state :: led_state(),
    off_file :: string(),
    pairable_signal :: ebus:filter_id(),
    miner_proxy :: ebus:proxy(),
    diagnostics = [] :: [{string(), string()}],
    diagnostics_timeout = make_ref() :: reference()
}).

lights_info() ->
    gen_server:call(?MODULE, lights_info).

lights_event(State) ->
    ?MODULE ! {lights_event, State}.

start_link(Bus) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Bus], [{hibernate_after, 10000}]).

init([Bus]) ->
    %% Trap exits to allow for terminate led animation
    erlang:process_flag(trap_exit, true),

    OffFile = application:get_env(led, lights_off, "/tmp/gateway_lights_off"),
    InitLedState =
        case filelib:is_regular(OffFile) of
            true -> disable;
            _ -> undefined
        end,

    LedPath = application:get_env(led, path, "/sys/bus/i2c/devices/1-0030"),
    LedHandle =
        case file:read_file_info(LedPath) of
            {ok, _} ->
                {ok, LS} = lp5562:init(LedPath),
                self() ! init_led,
                LS;
            _ ->
                lager:warning("No i2c device found, running in stub mode"),
                undefined
        end,

    {ok, BluezProxy} = ebus_proxy:start_link(Bus, ?BLUEZ_SERVICE, []),
    {ok, PairableSignal} = ebus_proxy:add_signal_handler(
        BluezProxy,
        ?BLUEZ_OBJECT_PATH,
        ?DBUS_PROPERTIES(?BLUEZ_MEMBER_PROPERTIES_CHANGED),
        self(),
        pairable_signal
    ),

    {ok, MinerProxy} = ebus_proxy:start_link(Bus, ?MINER_APPLICATION_NAME, []),

    State = #state{
        handle = LedHandle,
        off_file = OffFile,
        state = InitLedState,
        pairable_signal = PairableSignal,
        miner_proxy = MinerProxy
    },
    self() ! init_led,
    {ok, State}.

handle_call(lights_info, _From, State = #state{}) ->
    {reply, State#state.state, State};
handle_call(Msg, _From, State) ->
    lager:warning("Unhandled call ~p", [Msg]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    lager:warning("Unhandled cast ~p", [Msg]),
    {noreply, State}.

handle_info(init_led, State = #state{}) ->
    %% Don't update diagnostics for a cycle to give the system some time to settle
    DiagnosticsTimer = erlang:send_after(?DIAGNOSTICS_TIMEOUT, self(), diagnostics_timeout),
    NewState = State#state{diagnostics_timeout = DiagnosticsTimer},
    {noreply, handle_led_event(p2p_led_event(NewState#state.diagnostics), NewState)};
handle_info({ebus_signal, _Path, Signal, Msg}, State = #state{pairable_signal = Signal}) ->
    case ebus_message:args(Msg) of
        {ok, [?GATT_ADVERTISING_MANAGER_IFACE, #{"ActiveInstances" := Value}, _]} ->
            case Value of
                0 ->
                    {noreply, handle_led_event(stop_advert, State)};
                _ ->
                    {noreply, handle_led_event(start_advert, State)}
            end;
        _ ->
            {noreply, State}
    end;
handle_info({lights_event, Event}, State = #state{}) ->
    {noreply, handle_led_event(Event, State)};
handle_info(diagnostics_timeout, State = #state{}) ->
    Diagnostics = gateway_config:diagnostics(State#state.miner_proxy),
    State1 = handle_led_event(p2p_led_event(Diagnostics), State#state{diagnostics = Diagnostics}),
    DiagnosticsTimer = erlang:send_after(?DIAGNOSTICS_TIMEOUT, self(), diagnostics_timeout),
    {noreply, State1#state{diagnostics_timeout = DiagnosticsTimer}};
handle_info(Msg, State) ->
    lager:warning("Unhandled info ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, #state{}) ->
    ok.

%%
%% Internal
%%

-spec p2p_led_event([{string(), string()}]) -> online | offline.
p2p_led_event(Diagnostics) ->
    case proplists:get_value("connected", Diagnostics, "no") == "yes" of
        true -> online;
        _ -> offline
    end.

update_off_file(State = #state{state = disable}) ->
    file:write_file(State#state.off_file, <<>>),
    State;
update_off_file(State = #state{}) ->
    file:delete(State#state.off_file),
    State.

-spec update_led_state(led_event(), #state{}) -> #state{}.
update_led_state(Event, State = #state{state = Event}) ->
    %% No change in state
    State;
%% Panic
update_led_state(panic, State = #state{}) ->
    %% Enter panic mode
    State#state{state = panic};
update_led_state(_, State = #state{state = panic}) ->
    %% Once in panic mode, no events can rescue the state
    State;
%% Enable/Disable
update_led_state(disable, State = #state{}) ->
    update_off_file(State#state{state = disable});
update_led_state(enable, State = #state{}) ->
    update_off_file(
        update_led_state(p2p_led_event(State#state.diagnostics), State#state{state = undefined})
    );
update_led_state(_, State = #state{state = disable}) ->
    %% Ignore all events during disabled state
    State;
%% Start/Stop Advertising
update_led_state(start_advert, State = #state{state = {advert, _}}) ->
    %% Ignore new start_advert events to avoid nesting advert context
    State;
update_led_state(start_advert, State = #state{}) ->
    %% Remember the state when advertising begins. The adverttising
    %% start state determines what events can cause the advertising
    %% state to be exited prematurely.
    State#state{state = {advert, State#state.state}};
update_led_state(stop_advert, State = #state{state = {advert, _}}) ->
    %% We could set the state to the stored state here, but getting
    %% the actual value from p2p status seemed more error proof.
    update_led_state(p2p_led_event(State#state.diagnostics), State#state{state = undefined});
update_led_state(stop_advert, State = #state{}) ->
    %% Ignore stop_advert when not in advert
    State;
%% Online/Offline
update_led_state(online, State = #state{state = {advert, online}}) ->
    %% If we receive an online event while advertising and we were
    %% online when we started advertising, stay in advertising.
    State;
update_led_state(online, State = #state{state = {advert, _}}) ->
    %% If we receive an online event while advertising and we were not
    %% online when we started advertising, go to online state.
    State#state{state = online};
update_led_state(online, State = #state{}) ->
    State#state{state = online};
update_led_state(offline, State = #state{state = {advert, _}}) ->
    %% When in advertising mode, only "online" or the stop_advert
    %% timeout will get us out
    State;
update_led_state(offline, State = #state{}) ->
    State#state{state = offline};
%% Fallback fr all other events
update_led_state(_Event, State = #state{}) ->
    State#state{state = offline}.

-spec handle_led_event(Event :: term(), #state{}) -> #state{}.
handle_led_event(Event, State = #state{}) ->
    case update_led_state(Event, State) of
        NewState when NewState#state.state == State#state.state ->
            NewState;
        NewState ->
            update_led(NewState)
    end.

update_led(State = #state{state = panic}) ->
    led_set_color(?COLOR_RED, State);
update_led(State = #state{state = disable}) ->
    led_set_color(?COLOR_OFF, State);
update_led(State = #state{state = online}) ->
    led_set_color(?COLOR_GREEN, State);
update_led(State = #state{state = offline}) ->
    led_set_color(?COLOR_ORANGE, State);
update_led(State = #state{state = undefined}) ->
    led_set_color(?COLOR_ORANGE, State);
update_led(State = #state{state = {advert, _}}) ->
    led_set_color(?COLOR_BLUE, State).

led_set_color(_Color, State = #state{handle = undefined}) ->
    lager:info("Would have set LED: ~p", [_Color]),
    State;
led_set_color(Color, State) ->
    lp5562:set_color(Color, State#state.handle),
    State.
