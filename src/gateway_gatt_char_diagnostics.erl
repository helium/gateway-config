-module(gateway_gatt_char_diagnostics).
-include("gateway_gatt.hrl").
-include("gateway_config.hrl").
-include("pb/gateway_gatt_char_diagnostics_pb.hrl").

-behavior(gatt_characteristic).

-export([init/2,
         uuid/1,
         flags/1,
         read_value/2,
         handle_info/2]).

-export([diagnostics_to_bin/1]).

-record(state, { path :: ebus:object_path(),
                 proxy :: ebus:proxy(),
                 value= <<"init">> :: binary()
               }).


uuid(_) ->
    ?UUID_GATEWAY_GATT_CHAR_DIAGNOSTICS.

flags(_) ->
    [read].

init(Path, [Proxy]) ->
    Descriptors =
        [
         {gatt_descriptor_cud, 0, ["Diagnostics"]},
         {gatt_descriptor_pf, 1, [opaque]}
        ],
    {ok, Descriptors, #state{path=Path, proxy=Proxy}}.

read_value(State=#state{value=Value}, #{"offset" := Offset}) ->
    {ok, binary:part(Value, Offset, byte_size (Value) - Offset), State};
read_value(State=#state{}, _) ->
    Value = case gateway_config:diagnostics(State#state.proxy) of
                List when is_list(List), length(List) == 0 -> <<"wait">>;
                List when is_list(List), length(List) > 0 ->  diagnostics_to_bin(List);
                _ -> <<"unknown">>
            end,
    {ok, Value, State#state{value=Value}}.


diagnostics_to_bin(Diagnostics) ->
    Msg = #gateway_diagnostics_v1_pb{diagnostics=Diagnostics},
    gateway_gatt_char_diagnostics_pb:encode_msg(Msg).


 handle_info(Msg, State) ->
    lager:warning("Unhandled info ~p ~p",[Msg, State]),
    {noreply, State}.

%%
%% Internal
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

uuid_test() ->
    {ok, _, Char} = ?MODULE:init("", [proxy]),
    ?assertEqual(?UUID_GATEWAY_GATT_CHAR_DIAGNOSTICS, ?MODULE:uuid(Char)),
    ok.

flags_test() ->
    {ok, _, Char} = ?MODULE:init("", [proxy]),
    ?assertEqual([read], ?MODULE:flags(Char)),
    ok.

success_test() ->
    Path = "char_path",
    {ok, _, Char} = ?MODULE:init(Path, [proxy]),

    Diagnostics = [{"connected", "yes"},
                   {"dialable", "no"}],
    Msg = #gateway_diagnostics_v1_pb{diagnostics=Diagnostics},
    MsgBin = gateway_gatt_char_diagnostics_pb:encode_msg(Msg),

    meck:new(gateway_config, [passthrough]),
    meck:expect(gateway_config, diagnostics,
                fun(proxy) ->
                        Diagnostics
                end),

    {ok, ReadMsgBin, Char1} = ?MODULE:read_value(Char, #{}),
    ?assertEqual(ReadMsgBin, MsgBin),
    %% Reading it again just returns the same value
    ?assertEqual({ok, MsgBin, Char1}, ?MODULE:read_value(Char1, #{})),
    %% Reading at an offset returns the tail starting at offset
    ?assertEqual({ok, binary:part(MsgBin, {1, byte_size(MsgBin) - 1}), Char1},
                 ?MODULE:read_value(Char1, #{"offset" => 1})),

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
                fun(proxy) ->
                        Diagnostics
                end),

    {ok, MsgBin, _Char2} = ?MODULE:read_value(Char, #{}),

    ?assert(meck:validate(gateway_config)),
    meck:unload(gateway_config),

    ok.

-endif.
