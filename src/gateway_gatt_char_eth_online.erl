-module(gateway_gatt_char_eth_online).

-include("gateway_gatt.hrl").

-behavior(gatt_characteristic).

-export([
    init/2,
    uuid/1,
    flags/1,
    read_value/2
]).

-record(state, {path :: ebus:object_path()}).

uuid(_) ->
    ?UUID_GATEWAY_GATT_CHAR_ETH_ONLINE.

flags(_) ->
    [read].

init(Path, _) ->
    Descriptors = [
        {gatt_descriptor_cud, 0, ["Ethernet Online"]},
        {gatt_descriptor_pf, 1, [utf8_string]}
    ],
    {ok, Descriptors, #state{path = Path}}.

read_value(State = #state{}, _Opts) ->
    case gateway_config:ethernet_online() of
        true -> {ok, <<"true">>, State};
        false -> {ok, <<"false">>, State}
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

uuid_test() ->
    {ok, _, Char} = ?MODULE:init("", []),
    ?assertEqual(?UUID_GATEWAY_GATT_CHAR_ETH_ONLINE, ?MODULE:uuid(Char)),
    ok.

flags_test() ->
    {ok, _, Char} = ?MODULE:init("", []),
    ?assertEqual([read], ?MODULE:flags(Char)),
    ok.

%% nature of various unit tests indicate test environment is likely Wi-Fi
eth_offline_test() ->
    {ok, _, Char} = ?MODULE:init("", []),
    ?assertEqual(<<"false">>, ?MODULE:read_value(Char, #{})),
    ok.

-endif.
