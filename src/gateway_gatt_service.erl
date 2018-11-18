-module(gateway_gatt_service).
-include("gateway_gatt.hrl").

-behavior(gatt_service).

-export([init/1, uuid/1, handle_info/2]).

-record(state, {
                ssid="" :: string()
               }).

uuid(_) ->
    ?UUID_GATEWAY_GATT_SERVICE.

init(_) ->
    %% TODO: Connman crashing invalidates a number of pids that are
    %% used in characteristics. We should probably monitor and
    %% restart
    Characteristics =
        [
         {gateway_gatt_char_wifi_status, 0, []},
         {gateway_gatt_char_wifi_ssid, 1, []},
         {gateway_gatt_char_wifi_pass, 2, []}
        ],
    {ok, Characteristics, #state{}}.

handle_info({changed_wifi_ssid, Value}, State=#state{}) ->
    {noreply, State#state{ssid=binary_to_list(Value)}};
handle_info({changed_wifi_pass, _}, State=#state{ssid=""}) ->
    lager:notice("Not connecting to an empty SSID"),
    {noreply, State};
handle_info({changed_wifi_pass, Value}, State=#state{}) ->
    lager:info("Trying to connect to WiFI SSID: ~p", [State#state.ssid]),
    case connman:connect(wifi, State#state.ssid, binary_to_list(Value), self()) of
        ok -> ok;
        Other ->
            lager:notice("Start connect for SSID ~p: ~p", [State#state.ssid, Other])
    end,
    {noreply, State};
handle_info({connect_result, _Tech, Result}, State=#state{}) ->
    lager:info("Connect result ~p", [Result]),
    {noreply, State};
handle_info(Msg, State) ->
    lager:warning("Unhandled info ~p ~p",[Msg, State]),
    {noreply, State}.
