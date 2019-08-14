-module(gateway_gatt_char_diagnostics).
-include("gateway_gatt.hrl").
-include("gateway_config.hrl").
-include("pb/gateway_gatt_char_diagnostics_pb.hrl").

-behavior(gatt_characteristic).

-export([init/2,
         uuid/1,
         flags/1,
         read_value/2,
         start_notify/1,
         stop_notify/1,
         handle_info/2]).

-export([diagnostics_to_bin/1]).

-record(state, { path :: ebus:object_path(),
                 value= <<"init">> :: binary(),
                 notify=false :: boolean()
               }).


uuid(_) ->
    ?UUID_GATEWAY_GATT_CHAR_DIAGNOSTICS.

flags(_) ->
    [read, notify].

init(Path, _) ->
    Descriptors =
        [
         {gatt_descriptor_cud, 0, ["Diagnostics"]},
         {gatt_descriptor_pf, 1, [opaque]}
        ],
    gateway_config:diagnostics_join(self()),
    self() ! {diagnostics_char, Path},
    {ok, Descriptors,
     #state{path=Path}}.

read_value(State=#state{value=Value}, #{"offset" := Offset}) ->
    {ok, binary:part(Value, Offset, byte_size (Value) - Offset), State};
read_value(State=#state{value= <<"init">>}, _) ->
    Value = diagnostics_to_bin(gateway_config:diagnostics()),
    {ok, Value, State#state{value=Value}};
read_value(State=#state{}, _) ->
    {ok, State#state.value, State}.


diagnostics_to_bin(Status) ->
    Msg = #gateway_diagnostics_v1_pb{diagnostics=Status},
    gateway_gatt_char_diagnostics_pb:encode_msg(Msg).


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

handle_info({diagnostics, Status}, State) ->
    {noreply, maybe_notify_value(State#state{value=diagnostics_to_bin(Status)})};

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
    ?assertEqual(?UUID_GATEWAY_GATT_CHAR_DIAGNOSTICS, ?MODULE:uuid(Char)),
    ok.

flags_test() ->
    {ok, _, Char} = ?MODULE:init("", [proxy]),
    ?assertEqual([read, notify], ?MODULE:flags(Char)),
    ok.

success_test() ->
    Path = "char_path",
    {ok, _, Char} = ?MODULE:init(Path, [proxy]),

    meck:new(gatt_characteristic, [passthrough]),
    Diagnostics = [{"connected", "yes"},
                   {"dialable", "no"}],
    Msg = #gateway_diagnostics_v1_pb{diagnostics=Diagnostics},
    MsgBin = gateway_gatt_char_diagnostics_pb:encode_msg(Msg),

    UpdatedDiagnostics = [{"connected", "yes"},
                          {"dialable", "no"}],
    UpdatedMsg = #gateway_diagnostics_v1_pb{diagnostics=UpdatedDiagnostics},
    UpdatedMsgBin = gateway_gatt_char_diagnostics_pb:encode_msg(UpdatedMsg),

    meck:new(gateway_config, [passthrough]),
    meck:expect(gateway_config, diagnostics,
                fun() ->
                        Diagnostics
                end),
    meck:expect(gatt_characteristic, value_changed,
               fun(P, <<"init">>) when P == Path ->
                       ok;
                  (P, V) when P == Path, V == MsgBin ->
                       ok;
                  (P, V) when P == Path, V == UpdatedMsgBin ->
                       ok
               end),

    {ok, Char1} = ?MODULE:start_notify(Char),
    %% Calling start_notify again has no effect
    ?assertEqual({ok, Char1}, ?MODULE:start_notify(Char1)),

    {ok, ReadMsgBin, Char2} = ?MODULE:read_value(Char1, #{}),
    ?assertEqual(ReadMsgBin, MsgBin),
    %% Reading it again just returns the same value
    ?assertEqual({ok, MsgBin, Char2}, ?MODULE:read_value(Char2, #{})),
    %% Reading at an offset returns the tail starting at offset
    ?assertEqual({ok, binary:part(MsgBin, {1, byte_size(MsgBin) - 1}), Char2},
                 ?MODULE:read_value(Char2, #{"offset" => 1})),

    %% Deliver an update, which tests value notification through the
    %% mocked gatt_characteristic and the corresponding read below
    {noreply, Char3} = ?MODULE:handle_info({diagnostics, UpdatedDiagnostics}, Char2),
    ?assertEqual({ok, UpdatedMsgBin, Char3}, ?MODULE:read_value(Char3, #{})),

    {ok, Char4} = ?MODULE:stop_notify(Char3),
    ?assertMatch({ok, _}, ?MODULE:stop_notify(Char4)),

    ?assert(meck:validate(gatt_characteristic)),
    meck:unload(gatt_characteristic),

    ?assert(meck:validate(gateway_config)),
    meck:unload(gateway_config),

    ok.

error_test() ->
    Path = "char_path",
    {ok, _, Char} = ?MODULE:init(Path, [proxy]),

    %% failure to communicate with miner returns an empty list of checks
    Diagnostics = [],
    Msg = #gateway_diagnostics_v1_pb{diagnostics=Diagnostics},
    MsgBin = gateway_gatt_char_diagnostics_pb:encode_msg(Msg),

    meck:new(gateway_config, [passthrough]),
    meck:expect(gateway_config, diagnostics,
                fun() ->
                        Diagnostics
                end),

    {ok, MsgBin, _Char2} = ?MODULE:read_value(Char, #{}),

    ?assert(meck:validate(gateway_config)),
    meck:unload(gateway_config),

    ok.

-endif.
