-module(gateway_gatt_char_led_match).
-include("gateway_gatt.hrl").

-behavior(gatt_characteristic).

-export([init/2, uuid/1, flags/1,
         read_value/1, write_value/2,
         start_notify/1, stop_notify/1]).

-record(state, { path :: ebus:object_path(),
                 notify=false :: boolean(),
                 value :: binary()
               }).


uuid(#state{}) ->
    ?UUID_GATEWAY_GATT_CHAR_LED_MATCH.

flags(#state{}) ->
    [read, write, notify].

init(Path, _) ->
    Descriptors =
        [
         {gatt_descriptor_cud, 0, ["LED Match"]}
        ],
    {ok, Descriptors,
     #state{path=Path, value= <<"unknown">>}}.


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

read_value(State=#state{value=Value}) ->
    {ok, Value, State}.

write_value(State=#state{}, Bin) ->
    case (catch jsx:decode(Bin)) of
        {'EXIT', Reason} ->
            lager:warning("Failed to decode LED Match ~p: ~p", [Bin, Reason]);
        Tuples ->
            self() ! {changed_led_match, Tuples}
    end,
    {ok, maybe_notify_value(State#state{value=Bin})}.

%%
%% Internal
%%

maybe_notify_value(State=#state{notify=false}) ->
    State;
maybe_notify_value(State=#state{}) ->
    gatt_characteristic:value_changed(State#state.path,
                                      State#state.value).
