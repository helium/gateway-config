-module(gateway_gatt_char_wifi_connect).
-include("gateway_gatt.hrl").
-include("pb/gateway_gatt_char_wifi_connect_pb.hrl").

-behavior(gatt_characteristic).

-export([init/2,
         uuid/1,
         flags/1,
         read_value/2,
         write_value/2,
         start_notify/1,
         stop_notify/1,
         handle_info/2]).

-record(state, { path :: ebus:object_path(),
                 value= <<"init">> :: binary(),
                 notify=false :: boolean()
               }).


uuid(_) ->
    ?UUID_GATEWAY_GATT_CHAR_WIFI_CONNECT.

flags(_) ->
    [read, write, notify].

init(Path, _) ->
    Descriptors =
        [
         {gatt_descriptor_cud, 0, ["WiFi Connect"]},
         {gatt_descriptor_pf, 1, [opaque]}
        ],
    {ok, Descriptors,
     #state{path=Path}}.

read_value(State=#state{}, _) ->
    {ok, State#state.value, State}.

write_value(State=#state{}, Bin) ->
    try gateway_gatt_char_wifi_connect_pb:decode_msg(Bin, gateway_wifi_connect_v1_pb) of
        #gateway_wifi_connect_v1_pb{service=Service, password=Pass} ->
            self() ! {connect, wifi, Service, Pass, State#state.path},
            {ok, maybe_notify_value(State#state{value= <<"connecting">>})}
    catch _What:Why ->
            lager:warning("Failed to wifi_connect request: ~p", Why),
            {ok, maybe_notify_value(State#state{value= <<"badargs">>})}
    end.

start_notify(State=#state{notify=true}) ->
    %% Already notifying
    {ok, State};
start_notify(State=#state{}) ->
    {ok, maybe_notify_value(State#state{notify=true})}.


stop_notify(State=#state{notify=false}) ->
    %% Already not notifying
    {ok, State};
stop_notify(State=#state{}) ->
    {ok, State#state{notify=false}}.

handle_info({connect_result, wifi, Result}, State=#state{}) ->
    Value = case Result of
                ok -> <<"connected">>;
                {error, not_found} -> <<"not_found">>;
                {error, timeout} -> <<"timeout">>;
                {error, invalid_key} -> <<"invalid">>;
                {error, connect_failed} -> <<"failed">>;
                {error, already_connecting} -> <<"already">>;
                {error, _} -> <<"error">>
            end,
    {noreply, maybe_notify_value(State#state{value=Value})};

handle_info(Msg, State) ->
    lager:warning("Unhandled info ~p ~p",[Msg, State]),
    {noreply, State}.

%%
%% Internal
%%

maybe_notify_value(State=#state{notify=false}) ->
    State;
maybe_notify_value(State=#state{}) ->
    gatt_characteristic:value_changed(State#state.path,
                                      State#state.value),
    State.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

uuid_test() ->
    {ok, _, Char} = ?MODULE:init("", [proxy]),
    ?assertEqual(?UUID_GATEWAY_GATT_CHAR_WIFI_CONNECT, ?MODULE:uuid(Char)),
    ok.

flags_test() ->
    {ok, _, Char} = ?MODULE:init("", [proxy]),
    ?assertEqual([read, write, notify], ?MODULE:flags(Char)),
    ok.

success_test() ->
ValidValues =[
              <<"init">>,
              <<"connecting">>,
              <<"connected">>,
              <<"not_found">>,
              <<"timeout">>,
              <<"invalid">>,
              <<"already">>,
              <<"error">>
             ],

    meck:new(gatt_characteristic, [passthrough]),
    meck:expect(gatt_characteristic, value_changed,
               fun("", V) ->
                       ?assert(lists:member(V, ValidValues))
               end),

    {ok, _, Char} = ?MODULE:init("", [proxy]),

    {ok, Char1} = ?MODULE:start_notify(Char),
    %% Calling start_notify again has no effect
    ?assertEqual({ok, Char1}, ?MODULE:start_notify(Char1)),

    Req = #gateway_wifi_connect_v1_pb{service="test", password="pass"},
    ReqBin = gateway_gatt_char_wifi_connect_pb:encode_msg(Req),
    {ok, Char2} = ?MODULE:write_value(Char1, ReqBin),

    ?assertMatch({ok, <<"connecting">>, _}, ?MODULE:read_value(Char2, #{})),

    {noreply, Char3} = ?MODULE:handle_info({connect_result, wifi, ok}, Char2),
    ?assertMatch({ok, <<"connected">>, _}, ?MODULE:read_value(Char3, #{})),

    {ok, Char4} = ?MODULE:stop_notify(Char3),
    ?assertEqual({ok, Char4}, ?MODULE:stop_notify(Char4)),

    ?assert(meck:validate(gatt_characteristic)),
    meck:unload(gatt_characteristic),

    ok.

error_test() ->
    {ok, _, Char} = ?MODULE:init("", [proxy]),

    Char2 = lists:foldl(fun({Error, Value}, State) ->
                                {noreply, NewState} = ?MODULE:handle_info({connect_result, wifi, Error},
                                                                          State),
                                ?assertEqual({ok, Value, NewState}, ?MODULE:read_value(NewState, #{})),
                                NewState
                        end, Char,
                        [
                         {ok, <<"connected">>},
                         {{error, not_found}, <<"not_found">>},
                         {{error, timeout}, <<"timeout">>},
                         {{error, invalid_key}, <<"invalid">>},
                         {{error, already_connecting}, <<"already">>},
                         {{error, unknown}, <<"error">>}
                        ]),

    InvalidReqBin = <<"invalid">>,
    {ok, Char3} = ?MODULE:write_value(Char2, InvalidReqBin),
    ?assertEqual({ok, <<"badargs">>, Char3}, ?MODULE:read_value(Char3, #{})),

    {noreply, Char3} = ?MODULE:handle_info(unknown_message, Char3),
    ok.

-endif.
