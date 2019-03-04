-module(gateway_gatt_char_pubkey).
-include("gateway_gatt.hrl").
-include("gateway_config.hrl").

-behavior(gatt_characteristic).

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

init(Path, [Proxy]) ->
    Descriptors =
        [
         {gatt_descriptor_cud, 0, ["Public Key"]},
         {gatt_descriptor_pf, 1, [utf8_string]}
        ],
    {ok, Descriptors, #state{path=Path, proxy=Proxy}}.

read_value(State=#state{}) ->
    Value = case ebus_proxy:call(State#state.proxy, ?MINER_OBJECT(?MINER_MEMBER_PUBKEY)) of
                {ok, [PubKeyB58]} ->  list_to_binary(PubKeyB58);
                {error, "org.freedesktop.DBus.Error.ServiceUnknown"} ->
                    lager:warning("Miner not ready to get public key, returning waiting"),
                    <<"wait">>;
                {error, Error} ->
                    lager:warning("Failed to get public key, returning unkown: ~p", [Error]),
                    <<"unknown">>
            end,
    {ok, Value, State}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

uuid_test() ->
    {ok, _, Char} = ?MODULE:init("", [proxy]),
    ?assertEqual(?UUID_GATEWAY_GATT_CHAR_PUBKEY, ?MODULE:uuid(Char)),
    ok.

flags_test() ->
    {ok, _, Char} = ?MODULE:init("", [proxy]),
    ?assertEqual([read], ?MODULE:flags(Char)),
    ok.

read_test() ->
    {ok, _, Char} = ?MODULE:init("", [proxy]),

    PubKey = "pubkey",
    meck:expect(ebus_proxy, call,
                fun(proxy, ?MINER_OBJECT(?MINER_MEMBER_PUBKEY)) ->
                        {ok, [PubKey]}
                end),
    ?assertEqual({ok, list_to_binary(PubKey), Char}, ?MODULE:read_value(Char)),

   ?assert(meck:validate(ebus_proxy)),
    meck:unload(ebus_proxy),

    ok.

error_test() ->
    {ok, _, Char} = ?MODULE:init("", [proxy]),

    meck:expect(ebus_proxy, call,
                fun(proxy, ?MINER_OBJECT(?MINER_MEMBER_PUBKEY)) ->
                        ErrorName = get({?MODULE, meck_error}),
                        {error, ErrorName}
                end),

   lists:foldl(fun({ErrorName, Value}, State) ->
                       put({?MODULE, meck_error}, ErrorName),
                       ?assertEqual({ok, Value, State}, ?MODULE:read_value(State)),
                       State
               end, Char,
               [
                {"org.freedesktop.DBus.Error.ServiceUnknown", <<"wait">>},
                {"com.unknown.Error", <<"unknown">>}
               ]),

   ?assert(meck:validate(ebus_proxy)),
    meck:unload(ebus_proxy),

    ok.


-endif.
