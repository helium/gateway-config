-module(gateway_gatt_char_onboarding_key).

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
    ?UUID_GATEWAY_GATT_CHAR_ONBOARDING_KEY.

flags(_) ->
    [read].

init(Path, []) ->
    Descriptors = [
        {gatt_descriptor_cud, 0, ["Onboarding Key"]},
        {gatt_descriptor_pf, 1, [utf8_string]}
    ],
    {ok, Descriptors, #state{path = Path}}.

read_value(State = #state{}, _) ->
    Value =
        case gateway_config:get_public_key(onboarding_key) of
            {error, _} -> <<"unknown">>;
            {ok, V} -> list_to_binary(V)
        end,
    {ok, Value, State}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

uuid_test() ->
    {ok, _, Char} = ?MODULE:init("", []),
    ?assertEqual(?UUID_GATEWAY_GATT_CHAR_ONBOARDING_KEY, ?MODULE:uuid(Char)),
    ok.

flags_test() ->
    {ok, _, Char} = ?MODULE:init("", []),
    ?assertEqual([read], ?MODULE:flags(Char)),
    ok.

read_test() ->
    {ok, _, Char} = ?MODULE:init("", []),

    application:set_env(gateway_config, keys_file, "test/public_keys"),
    ?assertEqual({ok, <<"onboarding_key">>, Char}, ?MODULE:read_value(Char, #{})),

    ok.

error_test() ->
    {ok, _, Char} = ?MODULE:init("", []),

    application:set_env(gateway_config, keys_file, "test/no_keys_file"),
    ?assertEqual({ok, <<"unknown">>, Char}, ?MODULE:read_value(Char, #{})),

    ok.

-endif.
