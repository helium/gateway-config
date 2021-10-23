-module(gateway_gatt_char_wifi_configured_services).

-include("gateway_gatt.hrl").
-include("pb/gateway_gatt_char_wifi_services_pb.hrl").

-behavior(gatt_characteristic).

-export([
    init/2,
    uuid/1,
    flags/1,
    read_value/2
]).

-record(state, {
    path :: ebus:object_path(),
    value = <<>> :: binary()
}).

-define(MAX_VALUE_SIZE, 512).

uuid(_) ->
    ?UUID_GATEWAY_GATT_CHAR_WIFI_CONFIGURED_SERVICES.

flags(_) ->
    [read].

init(Path, _) ->
    Descriptors = [
        {gatt_descriptor_cud, 0, ["WiFi Configured Services"]},
        {gatt_descriptor_pf, 1, [utf8_string]}
    ],
    {ok, Descriptors, #state{path = Path}}.

read_value(State = #state{value = Value}, #{"offset" := Offset}) ->
    {ok, binary:part(Value, Offset, byte_size(Value) - Offset), State};
read_value(State = #state{}, _Opts) ->
    Services = gateway_config:wifi_services_configured(),
    case gateway_gatt_char_wifi_services:encode_services(Services) of
        {ok, _, Bin} -> {ok, Bin, State#state{value = Bin}};
        {error, _} -> {ok, <<"error">>, State#state{value = <<>>}}
    end.

%%
%% Internal
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

uuid_test() ->
    {ok, _, Char} = ?MODULE:init("", [proxy]),
    ?assertEqual(?UUID_GATEWAY_GATT_CHAR_WIFI_CONFIGURED_SERVICES, ?MODULE:uuid(Char)),
    ok.

flags_test() ->
    {ok, _, Char} = ?MODULE:init("", [proxy]),
    ?assertEqual([read], ?MODULE:flags(Char)),
    ok.

-endif.
