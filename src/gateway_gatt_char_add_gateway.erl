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
    ?UUID_GATEWAY_GATT_CHAR_ADD_GW.

flags(_) ->
    [read, write, notify].

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
                    lager:warning("Failed to get add gateway txn: ~p", [Error]),
                    case Error of
                        ?MINER_ERROR_BADARGS -> <<"badargs">>;
                        ?MINER_ERROR_GW_EXISTS -> <<"exists">>;
                        ?MINER_ERROR_INTERNAL -> <<"error">>;
                        _ -> <<"unknown">>
                    end
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


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

uuid_test() ->
    {ok, _, Char} = ?MODULE:init("", [proxy]),
    ?assertEqual(?UUID_GATEWAY_GATT_CHAR_ADD_GW, ?MODULE:uuid(Char)),
    ok.

flags_test() ->
    {ok, _, Char} = ?MODULE:init("", [proxy]),
    ?assertEqual([read, write, notify], ?MODULE:flags(Char)),
    ok.

success_test() ->
    Path = "char_path",
    {ok, _, Char} = ?MODULE:init(Path, [proxy]),
    Owner = "owner",
    OwnerBin = list_to_binary(Owner),
    BinTxn = <<"txn">>,

    meck:new(ebus_proxy, [passthrough]),
    meck:expect(ebus_proxy, call,
                fun(proxy, "/", ?MINER_OBJECT(?MINER_MEMBER_ADD_GW), [string], [B]) when B == Owner ->
                        {ok, [BinTxn]}
                end),
    meck:new(gatt_characteristic, [passthrough]),
    meck:expect(gatt_characteristic, value_changed,
               fun(P, <<"init">>) when P == Path ->
                       ok;
                  (P, V) when P == Path, V == BinTxn ->
                       ok
               end),

    {ok, Char1} = ?MODULE:start_notify(Char),
    %% Calling start_notify again has no effect
    ?assertEqual({ok, Char1}, ?MODULE:start_notify(Char1)),
    {ok, Char2} = ?MODULE:write_value(Char1, OwnerBin),
    ?assertEqual({ok, BinTxn, Char2}, ?MODULE:read_value(Char2)),

    {ok, Char3} = ?MODULE:stop_notify(Char2),
    ?assertEqual({ok, Char3}, ?MODULE:stop_notify(Char3)),

    ?assert(meck:validate(ebus_proxy)),
    meck:unload(ebus_proxy),
    ?assert(meck:validate(gatt_characteristic)),
    meck:unload(gatt_characteristic),

    ok.

error_test() ->
    Path = "char_path",
    {ok, _, Char} = ?MODULE:init(Path, [proxy]),

    meck:new(ebus_proxy, [passthrough]),
    meck:expect(ebus_proxy, call,
                fun(proxy, "/", ?MINER_OBJECT(?MINER_MEMBER_ADD_GW), [string], [B]) ->
                        {error, B}
                end),

    lists:foldl(fun({ErrorName, Value}, State) ->
                        {ok, NewState} = ?MODULE:write_value(State, list_to_binary(ErrorName)),
                        ?assertEqual({ok, Value, NewState}, ?MODULE:read_value(NewState)),
                        NewState
                  end, Char,
                 [
                  {?MINER_ERROR_BADARGS, <<"badargs">>},
                  {?MINER_ERROR_GW_EXISTS, <<"exists">>},
                  {?MINER_ERROR_INTERNAL, <<"error">>},
                  {"com.unknown.Error", <<"unknown">>}
                 ]),

    ?assert(meck:validate(ebus_proxy)),
    meck:unload(ebus_proxy),

    ok.


-endif.
