-module(gateway_gatt_char_wifi_credentials).
-include("gateway_gatt.hrl").
-include("pb/gateway_gatt_char_wifi_credentials_pb.hrl").

-behavior(gatt_characteristic).

-export([init/2,
         uuid/1,
         flags/1,
         read_value/2,
         encode_credentials/1,
         encode_credentials/2]).

-record(state, { path :: ebus:object_path(),
                 value= <<>> :: binary ()
               }).

-define(MAX_VALUE_SIZE, 512).

uuid(_) ->
    ?UUID_GATEWAY_GATT_CHAR_WIFI_CREDENTIALS.

flags(_) ->
    [read].

init(Path, _) ->
    Descriptors =
        [
         {gatt_descriptor_cud, 0, ["WiFi Credentials"]},
         {gatt_descriptor_pf, 1, [utf8_string]}
        ],
    {ok, Descriptors, #state{path=Path}}.

read_value(State=#state{value=Value}, #{"offset" := Offset}) ->
    {ok, binary:part(Value, Offset, byte_size (Value) - Offset), State};
read_value(State=#state{}, _Opts) ->
    Creds = gateway_config:wifi_credentials(),
    case encode_credentials(Creds) of
        {ok, _, Bin} -> {ok, Bin, State#state{value=Bin}};
        {error, _} -> {ok, <<"error">>, State#state{value= <<>>}}
    end.

encode_credentials(Creds) ->
    encode_credentials({Creds, []}, ?MAX_VALUE_SIZE).

encode_credentials({Creds, Tail}, MaxSize) ->
    Networks = lists:map(fun({Name, Pass}) -> #gateway_network_pb{name=Name, passphrase=Pass} end, Creds),
    Bin = gateway_gatt_char_wifi_credentials_pb:encode_msg(#gateway_wifi_credentials_v1_pb{credentials=Networks}),
    case byte_size(Bin) > MaxSize of
        true when length(Creds) == 1 ->
            {error, cred_length};
        true ->
            encode_credentials(lists:split(length(Creds) div 2, Creds), MaxSize);
        false when length(Tail) =< 1 ->
            {ok, Creds, Bin};
        false ->
            {Add, NewTail} = lists:split(length(Tail) div 2, Tail),
            encode_credentials({Creds ++ Add, NewTail}, MaxSize)
    end.

%%
%% Internal
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

uuid_test() ->
    {ok, _, Char} = ?MODULE:init("", [proxy]),
    ?assertEqual(?UUID_GATEWAY_GATT_CHAR_WIFI_CREDENTIALS, ?MODULE:uuid(Char)),
    ok.

flags_test() ->
    {ok, _, Char} = ?MODULE:init("", [proxy]),
    ?assertEqual([read], ?MODULE:flags(Char)),
    ok.

encode_test() ->
    Creds = [{"RT-AC66U-B1-38-2G", "solution_5663"}, {"NETGEAR97-5G", "!Fsgiw2c"}],
    case encode_credentials(Creds) of
        {ok, EncodedNetworks, _Encoded} ->
            ?assertEqual(length(EncodedNetworks), length(Creds));
        _ ->
            ?assert(false)
    end.

-endif.
