-module(gateway_gatt_char_wifi_services).

-include("gateway_gatt.hrl").
-include("pb/gateway_gatt_char_wifi_services_pb.hrl").

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-behavior(gatt_characteristic).

-export([
    init/2,
    uuid/1,
    flags/1,
    read_value/2,
    encode_services/1,
    encode_services/2
]).

-record(state, {
    path :: ebus:object_path(),
    value = <<>> :: binary()
}).

-define(MAX_VALUE_SIZE, 512).

uuid(_) ->
    ?UUID_GATEWAY_GATT_CHAR_WIFI_SERVICES.

flags(_) ->
    [read].

init(Path, _) ->
    Descriptors = [
        {gatt_descriptor_cud, 0, ["WiFi Services"]},
        {gatt_descriptor_pf, 1, [utf8_string]}
    ],
    {ok, Descriptors, #state{path = Path}}.

read_value(State = #state{value = Value}, #{"offset" := Offset}) ->
    {ok, binary:part(Value, Offset, byte_size(Value) - Offset), State};
read_value(State = #state{}, _Opts) ->
    Services = gateway_config:wifi_services(),
    Names = [Name || {Name, _} <- Services],
    case encode_services(Names) of
        {ok, _, Bin} -> {ok, Bin, State#state{value = Bin}};
        {error, _} -> {ok, <<"error">>, State#state{value = <<>>}}
    end.

-spec encode_services([string()]) -> {ok, [string()], binary()} | {error, term()}.
encode_services(Names) ->
    encode_services({Names, []}, ?MAX_VALUE_SIZE).

-spec encode_services({[string()], [string()]}, pos_integer()) ->
    {ok, [string()], binary()} | {error, term()}.
encode_services({Services, Tail}, MaxSize) ->
    Bin = encode_names(Services),
    case byte_size(Bin) > MaxSize of
        true when length(Services) == 1 ->
            {error, service_length};
        true ->
            encode_services(lists:split(length(Services) div 2, Services), MaxSize);
        false when length(Tail) =< 1 ->
            {ok, Services, Bin};
        false ->
            {Add, NewTail} = lists:split(length(Tail) div 2, Tail),
            encode_services({Services ++ Add, NewTail}, MaxSize)
    end.

-spec encode_names([string()]) -> binary().
encode_names(Names) ->
    Msg = #gateway_wifi_services_v1_pb{services = Names},
    gateway_gatt_char_wifi_services_pb:encode_msg(Msg).

-ifdef(TEST).

uuid_test() ->
    {ok, _, Char} = ?MODULE:init("", [proxy]),
    ?assertEqual(?UUID_GATEWAY_GATT_CHAR_WIFI_SERVICES, ?MODULE:uuid(Char)),
    ok.

flags_test() ->
    {ok, _, Char} = ?MODULE:init("", [proxy]),
    ?assertEqual([read], ?MODULE:flags(Char)),
    ok.

success_test() ->
    {ok, _, Char} = ?MODULE:init("", [proxy]),

    Services = [
        {"Verizon-MiFi7730L-F4C0", 70},
        {"LAKEHOUSE", 57},
        {"ARRIS-D2D7", 53},
        {"HP-Print-3D-Deskjet 3520 series", 43},
        {"Sun WiFi", 41},
        {"RVi-CC-a81b6a993575", 37},
        {"RVi-CC-6064059e03ac", 37}
    ],
    ServiceNames = [Name || {Name, _} <- Services],
    meck:new(gateway_config, [passthrough]),
    meck:expect(
        gateway_config,
        wifi_services,
        fun() -> Services end
    ),
    %% Ensure we can read the characteristic
    {ok, Bin, _Char2} = ?MODULE:read_value(Char, #{}),
    %% And that it decodes to the right list of names
    ?assertMatch(
        #gateway_wifi_services_v1_pb{services = ServiceNames},
        gateway_gatt_char_wifi_services_pb:decode_msg(Bin, gateway_wifi_services_v1_pb)
    ),

    ?assert(meck:validate(gateway_config)),
    meck:unload(gateway_config),
    ok.

error_test() ->
    {ok, _, Char} = ?MODULE:init("", [proxy]),
    Services = [{binary_to_list(crypto:strong_rand_bytes(?MAX_VALUE_SIZE + 1)), 80}],
    meck:new(gateway_config, [passthrough]),
    meck:expect(
        gateway_config,
        wifi_services,
        fun() -> Services end
    ),
    ?assertMatch({ok, <<"error">>, _}, ?MODULE:read_value(Char, #{})),

    ?assert(meck:validate(gateway_config)),
    meck:unload(gateway_config),

    ok.

to_range(M, N) ->
    Base = N div M,
    {Base * M, (Base + 1) * M}.

%% Test the properties of how we encode a list of service ids
prop_encode() ->
    ?FORALL(
        Names,
        service_id_list(),
        collect(
            to_range(10, length(Names)),
            begin
                case encode_services(Names) of
                    {ok, EncodedNames, Encoded} ->
                        %% If the list encodes successfully
                        {EncodedNames, Tail} = lists:split(length(EncodedNames), Names),
                        case Tail of
                            [] ->
                                %% and there is no remaining service
                                %% ids, the encoded list must be the
                                %% right size.
                                byte_size(Encoded) =< ?MAX_VALUE_SIZE andalso
                                    EncodedNames == Names;
                            _ ->
                                %% and there are remaining service
                                %% ids, then just adding one of the
                                %% remaining ones must push it past
                                %% the max size
                                EncodeOneMore = encode_names(EncodedNames ++ [hd(Tail)]),
                                lists:prefix(EncodedNames, Names) andalso
                                    byte_size(EncodeOneMore) > ?MAX_VALUE_SIZE
                        end;
                    {error, service_length} ->
                        %% If there was a encodeing failure, the
                        %% _first_ name must have been too long.
                        EncodedFirst = encode_names([hd(Names)]),
                        byte_size(EncodedFirst) > ?MAX_VALUE_SIZE
                end
            end
        )
    ).

%% Geneerate service ids that are mostly the right size, but
%% occasionally outrageously long.
service_id() ->
    frequency([
        {5, ?SIZED(Size, resize(1000 * Size, list(byte())))},
        {90, list(byte())}
    ]).

service_id_list() ->
    frequency([
        {5, ?SIZED(Size, resize(10 * Size, list(service_id())))},
        {90, list(service_id())}
    ]).

encode_test() ->
    ?assert(proper:quickcheck(prop_encode(), [long_result])),

    ok.

-endif.
