-module(gateway_gatt_char_assert_loc).

-include("gateway_gatt.hrl").
-include("gateway_config.hrl").
-include("pb/gateway_gatt_char_assert_loc_pb.hrl").

-behavior(gatt_characteristic).

-define(H3_LATLON_RESOLUTION, 12).

-export([
    init/2,
    uuid/1,
    flags/1,
    read_value/2,
    write_value/2,
    start_notify/1,
    stop_notify/1
]).

-record(state, {
    path :: ebus:object_path(),
    notify = false :: boolean(),
    value = <<"init">> :: binary()
}).

uuid(_) ->
    ?UUID_GATEWAY_GATT_CHAR_ASSERT_LOC.

flags(_) ->
    [read, write, notify].

init(Path, []) ->
    Descriptors = [
        {gatt_descriptor_cud, 0, ["Assert Location"]},
        {gatt_descriptor_pf, 1, [opaque]}
    ],
    {ok, Descriptors, #state{path = Path}}.

read_value(State = #state{value = Value}, #{"offset" := Offset}) ->
    {ok, binary:part(Value, Offset, byte_size(Value) - Offset), State};
read_value(State = #state{}, _) ->
    {ok, State#state.value, State}.

write_value(State = #state{}, Bin) ->
    try gateway_gatt_char_assert_loc_pb:decode_msg(Bin, gateway_assert_loc_v1_pb) of
        #gateway_assert_loc_v1_pb{
            lat = Lat,
            lon = Lon,
            owner = _Owner,
            nonce = _Nonce,
            fee = _Fee,
            amount = _Amount,
            payer = _Payer
        } ->
            H3Index = h3:from_geo({Lat, Lon}, ?H3_LATLON_RESOLUTION),
            H3String = h3:to_string(H3Index),
            lager:info(
                "Requesting unsupported assert_loc_txn for lat/lon/acc: {~p, ~p, ~p} index: ~p",
                [Lat, Lon, ?H3_LATLON_RESOLUTION, H3String]
            ),
            Value = <<"error">>,
            {ok, maybe_notify_value(State#state{value = Value})}
    catch
        _What:Why ->
            lager:warning("Failed to decode assert_loc request: ~p", [Why]),
            {ok, maybe_notify_value(State#state{value = <<"badargs">>})}
    end.

start_notify(State = #state{notify = true}) ->
    %% Already notifying
    {ok, State};
start_notify(State = #state{}) ->
    {ok, maybe_notify_value(State#state{notify = true})}.

stop_notify(State = #state{notify = false}) ->
    %% Already not notifying
    {ok, State};
stop_notify(State = #state{}) ->
    {ok, State#state{notify = false}}.

%%
%% Internal
%%

maybe_notify_value(State = #state{notify = false}) ->
    State;
maybe_notify_value(State = #state{}) ->
    gatt_characteristic:value_changed(
        State#state.path,
        State#state.value
    ),
    State.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

uuid_test() ->
    {ok, _, Char} = ?MODULE:init("", []),
    ?assertEqual(?UUID_GATEWAY_GATT_CHAR_ASSERT_LOC, ?MODULE:uuid(Char)),
    ok.

flags_test() ->
    {ok, _, Char} = ?MODULE:init("", []),
    ?assertEqual([read, write, notify], ?MODULE:flags(Char)),
    ok.

error_test() ->
    {ok, _, Char} = ?MODULE:init("", []),

    Req = #gateway_assert_loc_v1_pb{lat = 10.0, lon = 11.0},
    ReqBin = gateway_gatt_char_assert_loc_pb:encode_msg(Req),

    {ok, NewState} = ?MODULE:write_value(Char, ReqBin),
    ?assertEqual({ok, <<"error">>, NewState}, ?MODULE:read_value(NewState, #{})),

    ok.

-endif.
