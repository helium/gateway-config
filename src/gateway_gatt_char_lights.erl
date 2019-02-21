-module(gateway_gatt_char_lights).
-include("gateway_gatt.hrl").
-include("gateway_config.hrl").

-behavior(gatt_characteristic).

-export([init/2,
         uuid/1,
         flags/1,
         read_value/1,
         write_value/2,
         start_notify/1,
         stop_notify/1]).

-record(state, {
                path :: ebus:object_path(),
                notify=false :: boolean(),
                value= <<"on">> :: binary()
               }).


uuid(_) ->
    ?UUID_GATEWAY_GATT_CHAR_LIGHTS.

flags(_) ->
    [read, write, notify].

init(Path, []) ->
    Descriptors =
        [
         {gatt_descriptor_cud, 0, ["Lights"]},
         {gatt_descriptor_pf, 1, [utf8_string]}
        ],
    {ok, Descriptors, #state{path=Path}}.

read_value(State=#state{}) ->
    {ok, State#state.value, State}.

write_value(State=#state{}, Bin) ->
    Value = case Bin of
                <<"on">> -> <<"on">>;
                <<"off">> -> <<"off">>;
                _ -> State#state.value
            end,
    self() ! {lights, Value == <<"on">>},
    {ok, maybe_notify_value(State#state{value=Value})}.

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
    {ok, _, Char} = ?MODULE:init("", []),
    ?assertEqual(?UUID_GATEWAY_GATT_CHAR_LIGHTS, ?MODULE:uuid(Char)),
    ok.

flags_test() ->
    {ok, _, Char} = ?MODULE:init("", []),
    ?assertEqual([read, write, notify], ?MODULE:flags(Char)),
    ok.

off_test() ->
    {ok, _, Char} = ?MODULE:init("", []),

    meck:new(gatt_characteristic, [passthrough]),
    meck:expect(gatt_characteristic, value_changed,
               fun("", <<"on">>) ->
                       ok;
                  ("", V) when V == <<"off">> ->
                       ok
               end),

    {ok, Char1} = ?MODULE:start_notify(Char),
    %% Calling start_notify again has no effect
    ?assertEqual({ok, Char1}, ?MODULE:start_notify(Char1)),

    {ok, Char2} = ?MODULE:write_value(Char1, <<"off">>),
    ?assertEqual({ok, <<"off">>, Char2}, ?MODULE:read_value(Char2)),

    %% Write an invalid value and ensure off remains
    {ok, Char2} = ?MODULE:write_value(Char2, <<"invalid">>),
    ?assertEqual({ok, <<"off">>, Char2}, ?MODULE:read_value(Char2)),

    {ok, Char3} = ?MODULE:stop_notify(Char2),
    ?assertEqual({ok, Char3}, ?MODULE:stop_notify(Char3)),

    {ok, Char4} = ?MODULE:write_value(Char3, <<"on">>),
    ?assertEqual({ok, <<"on">>, Char4}, ?MODULE:read_value(Char4)),

    ?assert(meck:validate(gatt_characteristic)),
    meck:unload(gatt_characteristic),

    ok.



-endif.
