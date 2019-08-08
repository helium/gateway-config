-module(gateway_config_led).

-include_lib("gatt/include/gatt.hrl").
-include_lib("ebus/include/ebus.hrl").
-include("gateway_config.hrl").

-define(BLUEZ_OBJECT_PATH, "/org/bluez/hci0").
-define(BLUEZ_MEMBER_PROPERTIES_CHANGED, "PropertiesChanged").

-define(COLOR_RED,    {255,   0,   0}).
-define(COLOR_GREEN,  {  0, 255,   0}).
-define(COLOR_BLUE,   {  0,   0, 255}).
-define(COLOR_ORANGE, {255, 255,   0}).

-define(DIALABLE_TIMEOUT, 60 * 1000).

%% API
-export([lights_enable/1,
         lights_state/1,
         lights_info/0]).

%% gen_server
-export([start_link/1,
         init/1,
         handle_info/2,
         handle_call/3,
         handle_cast/2,
         terminate/2]).

-record(state, {
                handle=undefined :: lp5562:state() | undefined,
                state :: term(),
                enable :: boolean(),
                off_file :: string(),
                pairable_signal :: ebus:filter_id(),
                cached_dialable = false :: boolean(),
                dialable_timeout = make_ref() :: reference()
               }).

lights_enable(Enable) ->
    ?MODULE ! {enable_lights, Enable}.

lights_info() ->
    gen_server:call(?MODULE, lights_info).

lights_state(State) ->
    ?MODULE ! {lights_state, State}.

start_link(Bus) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Bus], []).


init([Bus]) ->
    %% Trap exits to allow for terminate led animation
    erlang:process_flag(trap_exit, true),

    OffFile = application:get_env(led, lights_off, "/tmp/gateway_lights_off"),
    Enable = not filelib:is_regular(OffFile),

    LedPath = application:get_env(led, path, "/sys/bus/i2c/devices/1-0030"),
    LedState = case file:read_file_info(LedPath) of
                 {ok, _} ->
                     {ok, LS} = lp5562:init(LedPath),
                     self() ! init_led,
                     LS;
              _ ->
                  lager:warning("No i2c device found, running in stub mode"),
                  undefined
          end,

    {ok, BluezProxy} = ebus_proxy:start_link(Bus, ?BLUEZ_SERVICE, []),
    {ok, PairableSignal} = ebus_proxy:add_signal_handler(BluezProxy,
                                                         ?BLUEZ_OBJECT_PATH,
                                                         ?DBUS_PROPERTIES(?BLUEZ_MEMBER_PROPERTIES_CHANGED),
                                                         self(), pairable_signal),

    State = #state{handle=LedState,
                   enable=Enable,
                   off_file=OffFile,
                   state=undefined,
                   pairable_signal=PairableSignal},

    case Enable of
        true -> self() ! init_led;
        false -> ok
    end,
    {ok, State}.


handle_call(lights_info, _From, State=#state{}) ->
    Lights = case State#state.enable of
                 true -> on;
                 false -> off
             end,
    {reply, {Lights, State#state.state}, State};

handle_call(Msg, _From, State) ->
    lager:warning("Unhandled call ~p", [Msg]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    lager:warning("Unhandled cast ~p", [Msg]),
    {noreply, State}.

handle_info(init_led, State=#state{}) ->
    {noreply, update_led(init_led_state(State), State)};
handle_info({ebus_signal, _Path, Signal, Msg}, State=#state{pairable_signal=Signal}) ->
    case ebus_message:args(Msg) of
        {ok, [?GATT_ADVERTISING_MANAGER_IFACE, #{"ActiveInstances" := Value}, _]} ->
            case Value of
                0 ->
                    {noreply, update_led(init_led_state(State), State)};
                _ ->
                    {noreply, update_led(start_advert, State)}
            end;
        _ ->
            {noreply, State}
    end;

handle_info({enable_lights, Enable}, State=#state{enable=Enable}) ->
    %% ignore same state
    {noreply, State};
handle_info({enable_lights, Enable}, State=#state{}) ->
    NewState0 = State#state{enable=Enable},
    NewState1 = update_led(init_led_state(NewState0), NewState0),
    {noreply, update_off_file(NewState1)};


handle_info({lights_state, LedState}, State=#state{}) ->
    NewState = update_led(LedState, State),
    {noreply, NewState};

handle_info({diagnostics, Diagnostics}, State=#state{}) ->
    State1 = update_cached_dialable(Diagnostics, State),
    State2 = update_led(p2p_led_state(Diagnostics, State1), State1),
    {noreply, State2};
handle_info(dialable_timeout, State=#state{}) ->
    %% On dialable timeout we stop relying on the cache and let the
    %% actual diagnostic take over the dialable value.
    {noreply, State#state{cached_dialable=false}};


handle_info(Msg, State) ->
    lager:warning("Unhandled info ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, #state{}) ->
    ok.


%%
%% Internal
%%

get_dialable(Diagnostics) ->
    proplists:get_value("dialable", Diagnostics, "no") == "yes".

get_connected(Diagnostics) ->
    proplists:get_value("connected", Diagnostics, "no") == "yes".

%% When the diagnostics state that this node is dialable we promote
%% that to cached_dialable and kick the timeout so that this dialable
%% state lasts for a while.
-spec update_cached_dialable([{string(), string()}], #state{}) -> #state{}.
update_cached_dialable(Diagnostics, State=#state{}) ->
    case get_dialable(Diagnostics) of
        true ->
            erlang:cancel_timer(State#state.dialable_timeout),
            Timer = erlang:send_after(?DIALABLE_TIMEOUT, self(), dialable_timeout),
            State#state{cached_dialable=true, dialable_timeout=Timer};
        false ->
            State
    end.

-spec p2p_led_state([{string(), boolean()}], #state{}) -> online | offline.
p2p_led_state(Diagnostics, State) ->
    %% The node is only onlike if it is connected and dialable. When
    %% not dialable we add some hysteresis with a cache since dialable
    %% can happen regulary with proxied connections.
    case {get_connected(Diagnostics), get_dialable(Diagnostics), State#state.cached_dialable} of
        {true, false, true} ->  online;
        {true, true, _} -> online;
        _ -> offline
    end.

update_off_file(State=#state{enable=true}) ->
    file:delete(State#state.off_file),
    State;
update_off_file(State=#state{enable=false}) ->
    file:write_file(State#state.off_file, <<>>),
    State.


-spec init_led_state(#state{}) -> term().
init_led_state(#state{enable=false}) ->
    disabled;
init_led_state(#state{state=panic}) ->
    panic;
init_led_state(State) ->
    %% If we're waiting to be online we check p2p status to get the
    %% online/offline value
    p2p_led_state(gateway_config:diagnostics(), State).


-spec update_led(LedState::term(), #state{}) -> #state{}.
%% No change
update_led(LedState, State=#state{state=LedState}) ->
    %% No change to led state
    State;
%% Online
update_led(LedState=online, State) ->
    led_set_color(?COLOR_GREEN, State#state{state=LedState});
update_led(LedState=offline, State) ->
    led_set_color(?COLOR_ORANGE, State#state{state=LedState});
%% Pairable
update_led(LedState=start_advert, State) ->
    led_set_color(?COLOR_BLUE, State#state{state=LedState});
%% Panic
update_led(LedState=panic, State) ->
    led_set_color(?COLOR_RED, State#state{state=LedState});
%% Fallback
update_led(LedState=_, State) ->
    %% Fallback state is offline
    led_blink(?COLOR_ORANGE, State#state{state=LedState}).


led_set_color(_Color, State=#state{handle=undefined}) ->
    lager:info("Would have set LED: ~p", [_Color]),
    State;
led_set_color(Color, State) ->
    lp5562:set_color(Color, State#state.handle),
    State.

led_blink(_Color, State=#state{handle=undefined}) ->
    lager:info("Would have blinked LED: ~p", [_Color]),
    State;
led_blink(Color, State=#state{}) ->
    lp5562:blink(Color, State#state.handle),
    State.
