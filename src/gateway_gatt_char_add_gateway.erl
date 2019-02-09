-module(gateway_gatt_char_add_gateway).
-include("gateway_gatt.hrl").
-include("gateway_config.hrl").

-behavior(gatt_characteristic).

-export([init/2,
         uuid/1,
         flags/1,
         read_value/1,
         write_value/2,
         start_notify/1,
         stop_notify/1]).

-record(state, {
                path :: ebus:object_path(),
                proxy :: ebus:proxy(),
                notify=false :: boolean(),
                value= <<"init">> :: binary()
               }).


uuid(_) ->
    ?UUID_GATEWAY_GATT_CHAR_PUBKEY.

flags(_) ->
    [read, notify].

init(Path, [Proxy]) ->
    Descriptors =
        [
         {gatt_descriptor_cud, 0, ["Add Gateway"]},
         {gatt_descriptor_pf, 1, [opaque]}
        ],
    {ok, Descriptors, #state{path=Path, proxy=Proxy}}.

read_value(State=#state{}) ->
    {ok, State#state.value, State}.

write_value(State=#state{}, Bin) ->
    Owner = binary_to_list(Bin),
    Value = case ebus_proxy:call(State#state.proxy, "/", ?MINER_OBJECT(?MINER_MEMBER_ADD_GW),
                                 [string], [Owner]) of
                {ok, [BinTxn]} ->  BinTxn;
                {error, Error} ->
                    lager:warning("Failed to get add gateway txn, returning error: ~p", [Error]),
                    <<"error">>
            end,
    {ok, maybe_notify_value(State#state{value=Value})}.

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


%%
%% Internal
%%

maybe_notify_value(State=#state{notify=false}) ->
    State;
maybe_notify_value(State=#state{}) ->
    gatt_characteristic:value_changed(State#state.path,
                                      State#state.value),
    State.
