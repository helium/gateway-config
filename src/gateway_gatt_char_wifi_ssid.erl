-module(gateway_gatt_char_wifi_ssid).

-include("gateway_gatt.hrl").

-behavior(gatt_characteristic).

-export([
    init/2,
    uuid/1,
    flags/1,
    read_value/2,
    start_notify/1,
    stop_notify/1,
    handle_signal/3
]).

-record(state, {
    path :: ebus:object_path(),
    notify = false :: boolean(),
    value = <<"">> :: binary(),
    wifi_signal :: ebus:filter_id(),
    service_path :: ebus:object_path() | undefined,
    service_signal = undefined :: ebubs:filter_id() | undefined
}).

uuid(_) ->
    ?UUID_GATEWAY_GATT_CHAR_WIFI_SSID.

flags(_) ->
    [read, notify].

init(Path, _) ->
    Descriptors = [
        {gatt_descriptor_cud, 0, ["WiFi SSID"]},
        {gatt_descriptor_pf, 1, [utf8_string]}
    ],
    {ok, SignalID} = connman:register_state_notify({tech, wifi}, self(), Path),
    %% Call update_value twice with different args to check for all possible states
    NextState = update_value(#state{path = Path, wifi_signal = SignalID}, "disconnect"),
    {ok, Descriptors, update_value(NextState, "online")}.

start_notify(State = #state{notify = true}) ->
    %% Already notifying
    {ok, State};
start_notify(State = #state{}) ->
    {ok, maybe_notify_value(State#state{notify = true})}.

stop_notify(State = #state{notify = false}) ->
    %% Already not notifying
    {ok, State};
stop_notify(State = #state{}) ->
    {ok, State#state{notify = false}}.

read_value(State = #state{value = Value}, _) ->
    {ok, Value, State}.

handle_signal(SignalID, Msg, State = #state{wifi_signal = SignalID}) ->
    %% WiFi interface state has changed
    case ebus_message:args(Msg) of
        {ok, ["Connected", true]} ->
            {noreply, maybe_notify_value(update_value(State, "online"))};
        {ok, ["Connected", false]} ->
            {noreply, maybe_notify_value(update_value(State, "disconnect"))};
        _ ->
            {noreply, State}
    end;
handle_signal(SignalID, Msg, State = #state{service_signal = SignalID}) ->
    %% Signal for the current service. Deal only with changes to the
    %% State attribute
    case ebus_message:args(Msg) of
        {ok, ["State", ServiceState]} ->
            {noreply, maybe_notify_value(update_value(State, ServiceState))};
        _ ->
            {noreply, State}
    end;
handle_signal(_SignalID, Msg, State = #state{}) ->
    %% State#state.service_signal /= _SignalID.
    %% Signal from a different service
    case ebus_message:args(Msg) of
        {ok, ["State", "online"]} ->
            NextState = maybe_notify_value(update_value(State, "disconnect")),
            {noreply, maybe_notify_value(update_value(NextState, "online"))};
        _ ->
            {noreply, State}
    end.

%%
%% Internal
%%

update_value(State = #state{service_path = CurrentPath}, ServiceState) ->
    %% Start a scan to repopulate list of visible wifi services
    connman:scan(wifi),
    case ServiceState of
        "online" ->
            maybe_register_state_notify(CurrentPath, State);
        _ ->
            maybe_unregister_state_notify(CurrentPath, State)
    end.

maybe_register_state_notify(CurrentPath, State) ->
    WiFiServicesOnline = gateway_config:wifi_services_online(),
    case {CurrentPath, WiFiServicesOnline} of
        {CurrentPath, [{_, CurrentPath} | _]} ->
            %% No change in path
            lager:info("WiFi unchanged for ~p at: ~p", [State#state.value, CurrentPath]),
            State;
        {undefined, [{Value, NewPath} | _]} ->
            %% From no path to a path
            lager:info("WiFi connected to: ~p at ~p", [Value, NewPath]),
            {ok, SignalID} = connman:register_state_notify(
                {path, NewPath},
                self(),
                State#state.path
            ),
            State#state{
                value = list_to_binary(Value),
                service_path = NewPath,
                service_signal = SignalID
            };
        {CurrentPath, [{Value, NewPath} | _]} ->
            %% Switching paths
            lager:info("WiFi changed from ~p to: ~p", [State#state.value, Value]),
            connman:unregister_state_notify(State#state.service_signal, self(), State#state.path),
            {ok, SignalID} = connman:register_state_notify(
                {path, NewPath},
                self(),
                State#state.path
            ),
            State#state{
                value = list_to_binary(Value),
                service_path = NewPath,
                service_signal = SignalID
            };
        _ ->
            lager:warning("early WiFi online event"),
            State
    end.

maybe_unregister_state_notify(CurrentPath, State) ->
    case CurrentPath of
        undefined ->
            %% No path and not connected
            lager:info("WiFi still disconnected"),
            State;
        CurrentPath ->
            ServiceSignal = State#state.service_signal,
            case ServiceSignal of
                undefined ->
                    State;
                _ ->
                    %% Disconnected from wifi
                    lager:info("WiFi disconnected from: ~p", [State#state.value]),
                    connman:unregister_state_notify(ServiceSignal, self(), State#state.path),
                    State#state{
                        service_signal = undefined,
                        value = <<"">>,
                        service_path = undefined
                    }
            end
    end.

maybe_notify_value(State = #state{notify = false}) ->
    State;
maybe_notify_value(State = #state{}) ->
    gatt_characteristic:value_changed(
        State#state.path,
        State#state.value
    ),
    State.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

meck_gateway_config() ->
    meck:new(gateway_config, [passthrough]),
    meck:expect(
        gateway_config,
        wifi_services_online,
        fun() ->
            case get({?MODULE, meck_services}) of
                undefined -> [];
                Val -> Val
            end
        end
    ).

meck_validate_gateway_config() ->
    ?assert(meck:validate(gateway_config)),
    meck:unload(gateway_config).

meck_connman_register_notify() ->
    meck:new(connman, [passthrough]),
    meck:expect(
        connman,
        register_state_notify,
        fun
            ({tech, wifi}, _, _) ->
                {ok, wifi_listener};
            ({path, _}, _, _) ->
                {ok, path_listener}
        end
    ).

meck_connman_scan() ->
    meck:expect(
        connman,
        scan,
        fun(wifi) ->
            ok
        end
    ).

meck_connman_unregister_notify() ->
    %% Don't mock the wifi_listener unregistration since we never
    %% unregister the wifi state listener
    meck:expect(
        connman,
        unregister_state_notify,
        fun(path_listener, _, _) ->
            ok
        end
    ).

meck_validate_connman() ->
    ?assert(meck:validate(connman)),
    meck:unload(connman).

uuid_test() ->
    meck_gateway_config(),
    meck_connman_register_notify(),
    meck_connman_scan(),

    {ok, _, Char} = ?MODULE:init("", [proxy]),
    ?assertEqual(?UUID_GATEWAY_GATT_CHAR_WIFI_SSID, ?MODULE:uuid(Char)),

    meck_validate_connman(),
    meck_validate_gateway_config(),
    ok.

flags_test() ->
    meck_gateway_config(),
    meck_connman_register_notify(),
    meck_connman_scan(),

    {ok, _, Char} = ?MODULE:init("", [proxy]),
    ?assertEqual([read, notify], ?MODULE:flags(Char)),

    meck_validate_connman(),
    meck_validate_gateway_config(),

    ok.

services_test() ->
    meck_connman_register_notify(),
    meck_connman_unregister_notify(),
    meck_connman_scan(),
    meck_gateway_config(),

    %% Set up one set of services
    Services = [{"OnlineService", "OnlinePath"}],
    put({?MODULE, meck_services}, Services),

    {ok, _, Char} = ?MODULE:init("", [proxy]),

    %% Start notifying
    {ok, Char2} = ?MODULE:start_notify(Char),
    %% Start notifying again hsa no impact
    ?assertEqual({ok, Char2}, ?MODULE:start_notify(Char2)),

    %% Read the value
    {ok, Result, Char3} = ?MODULE:read_value(Char2, #{}),
    ?assertEqual(list_to_binary(element(1, hd(Services))), Result),

    meck:new(ebus_message, [passthrough]),
    meck:expect(
        ebus_message,
        args,
        fun(_) ->
            {ok, get({?MODULE, meck_message_args})}
        end
    ),

    %% Try notifying and reading some new services
    Char4 = lists:foldl(
        fun({MsgArgs, NewServices}, State) ->
            put({?MODULE, meck_services}, NewServices),
            put({?MODULE, meck_message_args}, MsgArgs),
            {noreply, NewState} = ?MODULE:handle_signal(wifi_listener, ignore, State),
            ExpectedValue =
                case NewServices of
                    [] -> <<"">>;
                    _ -> list_to_binary(element(1, hd(NewServices)))
                end,
            %% The signal handler would already have refreshed the value, so
            %% no state change
            ?assertEqual({ok, ExpectedValue, NewState}, ?MODULE:read_value(NewState, #{})),
            NewState
        end,
        Char3,
        [
            %% Try setting twice with no services at all
            {["Connected", false], []},
            {["Connected", false], []},
            %% Set a new service with a new path
            {["Connected", true], [{"NewOnlineService", "OtherPath"}]},
            %% Then set again to ensure that setting the same path twice works
            {["Connected", true], [{"NewOnlineService", "OtherPath"}]}
        ]
    ),

    %% Stop notifying
    {ok, Char5} = ?MODULE:stop_notify(Char4),
    %% Stop notifying again has no impact
    ?assertEqual({ok, Char5}, ?MODULE:stop_notify(Char5)),

    _Char6 = lists:foldl(
        fun({MsgArgs, NewServices}, State) ->
            put({?MODULE, meck_services}, NewServices),
            put({?MODULE, meck_message_args}, MsgArgs),
            {noreply, NewState} = ?MODULE:handle_signal(path_listener, ignore, State),
            ExpectedValue = list_to_binary(element(1, hd(NewServices))),
            %% The signal handler would already have refreshed the value, so
            %% no state change
            ?assertEqual({ok, ExpectedValue, NewState}, ?MODULE:read_value(NewState, #{})),
            NewState
        end,
        Char5,
        [
            %% Set a new service with a new path
            {["State", "online"], [{"StateOnlineService", "StatePath"}]},
            %% Set with no actual state change.. the
            %% service list below will not be read but
            %% will be validated against.
            {["NotState", "bar"], [{"StateOnlineService", "StatePath"}]}
        ]
    ),

    meck_validate_connman(),
    meck_validate_gateway_config(),
    ?assert(meck:validate(ebus_message)),
    meck:unload(ebus_message),

    ok.

-endif.
