-module(gateway_gatt_char_wifi_status).
-include("gateway_gatt.hrl").

-behavior(gatt_characteristic).

-export([init/2, uuid/1, flags/1,
         read_value/1, write_value/2,
         start_notify/1, stop_notify/1,
         handle_signal/3]).

-record(state, { path :: ebus:object_path(),
                 notify=false :: boolean(),
                 value :: binary(),
                 wifi_signal :: ebus:filter_id()
               }).


uuid(_) ->
    ?UUID_GATEWAY_GATT_CHAR_WIFI_STATUS.

flags(_) ->
    [read, notify].

init(Path, _) ->
    %% Currently online status is only determined by the wifi state
    {ok, SignalID} = connman:register_state_notify({tech, wifi}, self(), Path),
    {ok, Value} = connman:state({tech, wifi}),
    Descriptors =
        [
         {gatt_descriptor_cud, 0, ["WiFi Status"]},
         {gatt_descriptor_pf, 1, [utf8_string]}
        ],
    {ok, Descriptors,
     #state{path=Path, value=value_to_binary(Value), wifi_signal=SignalID}}.

start_notify(State=#state{notify=true}) ->
    %% Already notifying
    {ok, State};
start_notify(State=#state{}) ->
    {ok, maybe_notify_value(State#state{notify=true})}.

stop_notify(State=#state{notify=false}) ->
    %% Already not notifying
    {ok, State};
stop_notify(State=#state{}) ->
    {ok, State#state{notify=false}}.

read_value(State=#state{value=Value}) ->
    {ok, Value, State}.

write_value(State=#state{}, Bin) ->
    {ok, maybe_notify_value(State#state{value=Bin})}.

handle_signal(SignalID, Msg, State=#state{wifi_signal=SignalID}) ->
    %% Wifi state changed
    case ebus_message:args(Msg) of
        {ok, ["Connected", Value]} ->
            lager:info("WiFi connected property changed to ~p", [Value]),
            {ok, NewState} = write_value(State, value_to_binary(Value)),
            {noreply, NewState};
        {ok, _} ->
            {noreply, State};
        {error, Error} ->
            lager:notice("Error decoding WiFi state: ~p", [Error]),
            {noreply, State}
    end.


%%
%% Internal
%%

maybe_notify_value(State=#state{notify=false}) ->
    State;
maybe_notify_value(State=#state{}) ->
    gatt_characteristic:value_changed(State#state.path,
                                      State#state.value),
    State.

value_to_binary(true) ->
    value_to_binary(online);
value_to_binary(online) ->
    <<"online">>;
value_to_binary(false) ->
    value_to_binary(idle);
value_to_binary(idle) ->
    <<"idle">>;
value_to_binary(disabled) ->
    <<"disabled">>.
