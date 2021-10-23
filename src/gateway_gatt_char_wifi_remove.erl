-module(gateway_gatt_char_wifi_remove).

-include("gateway_gatt.hrl").
-include("pb/gateway_gatt_char_wifi_remove_pb.hrl").

-behavior(gatt_characteristic).

-export([
    init/2,
    uuid/1,
    flags/1,
    write_value/2,
    handle_info/2
]).

-record(state, {path :: ebus:object_path()}).

uuid(_) ->
    ?UUID_GATEWAY_GATT_CHAR_WIFI_REMOVE.

flags(_) ->
    [write].

init(Path, _) ->
    Descriptors = [
        {gatt_descriptor_cud, 0, ["WiFi Remove"]},
        {gatt_descriptor_pf, 1, [opaque]}
    ],
    {ok, Descriptors, #state{path = Path}}.

write_value(State = #state{}, Bin) ->
    try gateway_gatt_char_wifi_remove_pb:decode_msg(Bin, gateway_wifi_remove_v1_pb) of
        #gateway_wifi_remove_v1_pb{service = Service} ->
            self() ! {remove, wifi, Service, State#state.path},
            {ok, State}
    catch
        _What:Why ->
            lager:warning("Failed to parse wifi_remove request: ~p", [Why]),
            {ok, State}
    end.

handle_info(Msg, State) ->
    lager:warning("Unhandled info ~p ~p", [Msg, State]),
    {noreply, State}.

%%
%% Internal
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

uuid_test() ->
    {ok, _, Char} = ?MODULE:init("", [proxy]),
    ?assertEqual(?UUID_GATEWAY_GATT_CHAR_WIFI_REMOVE, ?MODULE:uuid(Char)),
    ok.

flags_test() ->
    {ok, _, Char} = ?MODULE:init("", [proxy]),
    ?assertEqual([write], ?MODULE:flags(Char)),
    ok.

success_test() ->
    meck:new(gatt_characteristic, [passthrough]),

    {ok, _, Char} = ?MODULE:init("", [proxy]),

    Req = #gateway_wifi_remove_v1_pb{service = "test"},
    ReqBin = gateway_gatt_char_wifi_remove_pb:encode_msg(Req),
    {ok, _Char} = ?MODULE:write_value(Char, ReqBin),

    ?assert(meck:validate(gatt_characteristic)),
    meck:unload(gatt_characteristic),

    ok.

-endif.
