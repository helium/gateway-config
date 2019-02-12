-module(gateway_gatt_char_assert_loc).
-include("gateway_gatt.hrl").
-include("gateway_config.hrl").
-include("pb/gateway_gatt_char_assert_loc_pb.hrl").

-behavior(gatt_characteristic).

-define(H3_LATLON_RESOLUTION, 12).

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
    ?UUID_GATEWAY_GATT_CHAR_ASSERT_LOC.

flags(_) ->
    [read, notify].

init(Path, [Proxy]) ->
    Descriptors =
        [
         {gatt_descriptor_cud, 0, ["Assert Location"]},
         {gatt_descriptor_pf, 1, [opaque]}
        ],
    {ok, Descriptors, #state{path=Path, proxy=Proxy}}.

read_value(State=#state{}) ->
    {ok, State#state.value, State}.

write_value(State=#state{}, Bin) ->
    case (catch gateway_gatt_char_assert_loc_pb:decode_msg(Bin, gateway_assert_loc_v1_pb)) of
        #gateway_assert_loc_v1_pb{lat=Lat, lon=Lon} ->
            H3Index = h3:from_geo({Lat, Lon}, ?H3_LATLON_RESOLUTION),
            Value = case ebus_proxy:call(State#state.proxy, "/", ?MINER_OBJECT(?MINER_MEMBER_ASSERT_LOC),
                                         [uint64], [H3Index]) of
                        {ok, [BinTxn]} ->  BinTxn;
                        {error, Error} ->
                            lager:warning("Failed to get assert_loc txn: ~p", [Error]),
                            case Error of
                                ?MINER_ERROR_BADARGS -> <<"badargs">>;
                                ?MINER_ERROR_INTERNAL -> <<"error">>;
                                ?MINER_ERROR_GW_NOT_FOUND -> <<"gw_not_found">>;
                                ?MINER_ERROR_ASSERT_LOC_PARENT -> <<"assert_loc_parent">>;
                                ?MINER_ERROR_ASSERT_LOC_EXISTS -> <<"assert_loc_exists">>;
                                _ -> <<"unknown">>
                            end
                    end,
            {ok, maybe_notify_value(State#state{value=Value})};
        {'EXIT', Why} ->
            lager:warning("Failed to decode assert_loc request: ~p", Why),
            {ok, maybe_notify_value(State#state{value= <<"badargs">>})}
    end.

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
