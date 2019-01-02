-module(gateway_gatt_char_qr_code_status).
-include("gateway_gatt.hrl").
-include("gateway_config.hrl").

-behavior(gatt_characteristic).

-export([init/2, uuid/1, flags/1,
         read_value/1, write_value/2,
         start_notify/1, stop_notify/1,
         handle_signal/3]).

-record(state, { path :: ebus:object_path(),
                 notify=false :: boolean(),
                 value :: binary(),
                 add_signal :: ebus:filter_id(),
                 status_signal :: ebus:filter_id()
               }).


uuid(_) ->
    ?UUID_GATEWAY_GATT_CHAR_QR_CODE_STATUS.

flags(_) ->
    [read, notify].

init(Path, _) ->
    {ok, Bus} = ebus:system(),
    {ok, Proxy} = ebus_proxy:start_link(Bus, ?CONFIG_APPLICATION_NAME, []),
    %% Register a signal handler for the QR code being scanned. This
    %% is a cleaner separation than the root gatt service trying to
    %% needle a message down here.
    {ok, AddSignal} = ebus_proxy:add_signal_handler(Proxy,
                                                    ?CONFIG_OBJECT(?CONFIG_MEMBER_ADD_GW),
                                                    self(), Path),
    %% Register a signal handler for the gateway add status. This is
    %% sent by the miner when it tries to send the add gateway request
    %% to the wallet. The string in the signal is the current
    %% status. See gateway_config.hrl for possible values.
    {ok, StatusSignal} = ebus_proxy:add_signal_handler(Proxy,
                                                       ?MINER_OBJECT(?MINER_MEMBER_ADD_GW_STATUS),
                                                       self(), Path),
    Descriptors =
        [
         {gatt_descriptor_cud, 0, ["QR Code Status"]},
         {gatt_descriptor_pf, 1, [utf8_string]}
        ],
    {ok, Descriptors,
     #state{path=Path, value=value_to_binary("init"),
            add_signal=AddSignal, status_signal=StatusSignal}}.

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

handle_signal(AddSignal, _Msg, State=#state{add_signal=AddSignal}) ->
    %% QR code was scanned and signaled out. Reset the status to received
    {ok, NewState} = write_value(State, value_to_binary("received")),
    {noreply, NewState};
handle_signal(StatusSignal, Msg, State=#state{status_signal=StatusSignal}) ->
    %% add gateway request submitted to wallet. Check the signal
    %% argument if the submission succeeded (true) or failed (false)
    case ebus_message:args(Msg) of
        {ok, [Value]} when is_list(Value) ->
            lager:info("QR code submitted signal receipt: ~p", [Value]),
            {ok, NewState} = write_value(State, value_to_binary(Value)),
            {noreply, NewState};
        {ok, Other} ->
            lager:notice("Invalid QR submitted signal content ~p", [Other]),
            {noreply, State};
        {error, Error} ->
            lager:notice("Error decoding QR code submitted signal: ~p", [Error]),
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

value_to_binary(Str) ->
    list_to_binary(Str).
