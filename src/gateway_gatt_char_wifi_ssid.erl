-module(gateway_gatt_char_wifi_ssid).
-include("gateway_gatt.hrl").

-behavior(gatt_characteristic).

-export([init/2, uuid/1, flags/1,
         read_value/1,
         start_notify/1, stop_notify/1,
         handle_signal/3]).

-record(state, { path :: ebus:object_path(),
                 notify=false :: boolean(),
                 value=undefined :: binary() | undefined,
                 wifi_signal=undefined :: ebus:filter_id() | undefined
               }).


uuid(_) ->
    ?UUID_GATEWAY_GATT_CHAR_WIFI_SSID.

flags(_) ->
    [read, notify].

init(Path, _) ->
    Descriptors =
        [
         {gatt_descriptor_cud, 0, ["WiFi SSID"]},
         {gatt_descriptor_pf, 1, [utf8_string]}
        ],
    {ok, Descriptors, #state{path=Path}}.

start_notify(State=#state{notify=true}) ->
    %% Already notifying
    {ok, State};
start_notify(State=#state{}) ->
    {ok, SignalID} = connman:register_state_notify({tech, wifi}, self(), State#state.path),
    {ok, maybe_notify_value(State#state{notify=true, wifi_signal=SignalID})}.

stop_notify(State=#state{notify=false}) ->
    %% Already not notifying
    {ok, State};
stop_notify(State=#state{}) ->
    connman:unregister_state_notify(State#state.wifi_signal, self(), State#state.path),
    {ok, State#state{notify=false, wifi_signal=undefined}}.

read_value(State=#state{value=undefined}) ->
    read_value(State#state{value=online_value()});
read_value(State=#state{value=Value}) ->
    {ok, Value, State}.


handle_signal(SignalID, _Msg, State=#state{wifi_signal=SignalID}) ->
    %% Wifi state changed
    Value = online_value(),
    lager:info("WiFi SSID property changed to ~p", [Value]),
    {noreply, maybe_notify_value(State#state{value=Value})}.
%%
%% Internal
%%

maybe_notify_value(State=#state{notify=false}) ->
    State;
maybe_notify_value(State=#state{}) ->
    gatt_characteristic:value_changed(State#state.path,
                                      State#state.value),
    State.


online_value() ->
    case gateway_config:wifi_services_online() of
        [] -> <<"">>;
        [Service | _] -> list_to_binary(Service)
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

uuid_test() ->
    {ok, _, Char} = ?MODULE:init("", [proxy]),
    ?assertEqual(?UUID_GATEWAY_GATT_CHAR_WIFI_SSID, ?MODULE:uuid(Char)),
    ok.

flags_test() ->
    {ok, _, Char} = ?MODULE:init("", [proxy]),
    ?assertEqual([read, notify], ?MODULE:flags(Char)),
    ok.

services_test() ->
    {ok, _, Char} = ?MODULE:init("", [proxy]),

    meck:new(gateway_config, [passthrough]),
    meck:expect(gateway_config, wifi_services_online,
                fun() ->
                        get({?MODULE, meck_services})
                end),
    meck:new(connman, [passthrough]),
    meck:expect(connman, register_state_notify,
               fun(_, _, _) -> {ok, listener} end),
    meck:expect(connman, unregister_state_notify,
               fun(listener, _, _) -> ok end),

    %% Set up one set of services
    Services = ["OnlineService"],
    put({?MODULE, meck_services}, Services),

    %% Start notifying
    {ok, Char2} = ?MODULE:start_notify(Char),
    %% Start notifying again hsa no impact
    ?assertEqual({ok, Char2}, ?MODULE:start_notify(Char2)),

    %% Read the value
    {ok, Result, Char3} = ?MODULE:read_value(Char2),
    ?assertEqual(Result, list_to_binary(hd(Services))),

    %% Try notifying and reading some new services
    Char4 = lists:foldl(fun(NewServices, State) ->
                                put({?MODULE, meck_services}, NewServices),
                                {noreply, NewState} = ?MODULE:handle_signal(listener, ignore, State),
                                ExpectedValue = case NewServices of
                                                    [] -> <<"">>;
                                                    _ -> list_to_binary(hd(NewServices))
                                                end,
                                %% The signal handler would already have refreshed the value, so
                                %% no state change
                                ?assertEqual({ok, ExpectedValue, NewState}, ?MODULE:read_value(NewState)),
                                NewState
                        end, Char3,
                        [
                         ["NewOnlineServices"],
                         []
                        ]),

    %% Stop notifying
    {ok, Char5} = ?MODULE:stop_notify(Char4),
    %% Stop notifying again has no impact
    ?assertEqual({ok, Char5}, ?MODULE:stop_notify(Char5)),
    %% Hack
    ?assertEqual(Char5, maybe_notify_value(Char5)),

    ?assert(meck:validate(gateway_config)),
    meck:unload(gateway_config),

    ?assert(meck:validate(connman)),
    meck:unload(connman),

    ok.


-endif.
