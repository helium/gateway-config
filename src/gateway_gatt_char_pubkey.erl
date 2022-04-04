-module(gateway_gatt_char_pubkey).

-include("gateway_gatt.hrl").
-include("gateway_config.hrl").

-behavior(gatt_characteristic).

-export([
    init/2,
    uuid/1,
    flags/1,
    read_value/2
]).

-record(state, {
    path :: ebus:object_path()
}).

uuid(_) ->
    ?UUID_GATEWAY_GATT_CHAR_PUBKEY.

flags(_) ->
    [read].

init(Path, []) ->
    Descriptors = [
        {gatt_descriptor_cud, 0, ["Public Key"]},
        {gatt_descriptor_pf, 1, [utf8_string]}
    ],
    {ok, Descriptors, #state{path = Path}}.

read_value(State = #state{}, _) ->
    Value =
        case gateway_config:get_public_key(pubkey) of
            {error, _} -> <<"unknown">>;
            {ok, V} -> list_to_binary(V)
        end,
    {ok, Value, State}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

uuid_test() ->
    {ok, _, Char} = ?MODULE:init("", []),
    ?assertEqual(?UUID_GATEWAY_GATT_CHAR_PUBKEY, ?MODULE:uuid(Char)),
    ok.

flags_test() ->
    {ok, _, Char} = ?MODULE:init("", []),
    ?assertEqual([read], ?MODULE:flags(Char)),
    ok.

read_test() ->
    {ok, _, Char} = ?MODULE:init("", []),

    application:set_env(gateway_config, keys_file, "test/public_keys"),
    ?assertEqual(
        {ok, <<"112YR4mqBrc6DxrVonKsYE6bb6TGab3K1vVfpF8yAebRSd5YgZDy">>, Char},
        ?MODULE:read_value(Char, #{})
    ),

    ok.

error_test() ->
    {ok, _, Char} = ?MODULE:init("", []),

    application:set_env(gateway_config, keys_file, "test/no_keys_file"),
    ?assertEqual({ok, <<"unknown">>, Char}, ?MODULE:read_value(Char, #{})),

    ok.

-endif.
