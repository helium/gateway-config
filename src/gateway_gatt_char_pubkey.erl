-module(gateway_gatt_char_pubkey).
-include("gateway_gatt.hrl").

-behavior(gatt_characteristic).

-define(MINER_APPLICATION_NAME, "com.helium.Miner").
-define(MINER_INTERFACE, "com.helium.Miner").
-define(MINER_OBJECT(M), ?MINER_INTERFACE ++ "." ++ M).
-define(MINER_MEMBER_PUBKEY, "PubKey").

-export([init/2,
         uuid/1,
         flags/1,
         read_value/1]).

-record(state, {
                 path :: ebus:object_path(),
                 proxy :: ebus:proxy()
               }).


uuid(_) ->
    ?UUID_GATEWAY_GATT_CHAR_PUBKEY.

flags(_) ->
    [read].

init(Path, _) ->
    {ok, Bus} = ebus:system(),
    {ok, Proxy} = ebus_proxy:start_link(Bus, ?MINER_APPLICATION_NAME, []),
    Descriptors =
        [
         {gatt_descriptor_cud, 0, ["Public Key"]},
         {gatt_descriptor_pf, 1, [utf8_string]}
        ],
    {ok, Descriptors, #state{path=Path, proxy=Proxy}}.

read_value(State=#state{}) ->
    Value = case ebus_proxy:call(State#state.proxy, ?MINER_OBJECT(?MINER_MEMBER_PUBKEY)) of
                {ok, [PubKeyB58]} ->  list_to_binary(PubKeyB58);
                {error, Error} ->
                    lager:warning("Failed to get public key, returning unkown: ~p", [Error]),
                    <<"unknown">>
            end,
    {ok, Value, State}.
