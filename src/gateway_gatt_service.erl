-module(gateway_gatt_service).
-include("gateway_gatt.hrl").

-behavior(gatt_service).

-export([init/1, uuid/0, handle_info/2]).

-record(state, {
                ssid="" :: string()
               }).

uuid() ->
    ?UUID_GATEWAY_GATT_SERVICE.

init(_) ->
    %% TODO: Connman crashing invalidates a number of pids that are
    %% used in characteristics. We should probably monitor and
    %% restart
    Characteristics =
        [
         {gateway_gatt_char_wifi_status, 0, []},
         {gateway_gatt_char_wifi_ssid, 1, []},
         {gateway_gatt_char_wifi_pass, 2, []},
         {gateway_gatt_char_qr_code, 3, []},
         {gateway_gatt_char_qr_code_status, 4, []},
         {gatt_characteristic_string, 5, [{uuid, ?UUID_GATEWAY_GATT_CHAR_MAC},
                                          {value, gateway_config:serial_number()}]},
         {gateway_gatt_char_wifi_services, 6, []},
         {gateway_gatt_char_pubkey, 7, []}
        ],
    self() ! enable_wifi,
    {ok, Characteristics, #state{}}.

handle_info(enable_wifi, State=#state{}) ->
    connman:enable(wifi, true),
    {noreply, State};
handle_info({changed_wifi_ssid, Value}, State=#state{}) ->
    {noreply, State#state{ssid=binary_to_list(Value)}};
handle_info({changed_wifi_pass, _}, State=#state{ssid=""}) ->
    lager:notice("Not connecting to an empty SSID"),
    {noreply, State};
handle_info({changed_wifi_pass, Value}, State=#state{}) ->
    %% To aid the gatt online notifications we fetch all services that
    %% are wifi and online or ready and attempt to disconnect them
    %% before we try to connect to the SSID stored in the state.
    OnlineWifiPaths =
        lists:filtermap(fun({Path, M}) ->
                                lager:info("Checking wifi ~p", [Path]),
                                case maps:get("Type", M, false) == "wifi" andalso
                                    lists:member(maps:get("State", M, false), ["online", "ready"]) of
                                    true -> {true, maps:get("Name", M)};
                                    false -> false
                                end
                        end, connman:services()),
    lists:foreach(fun(Name) ->
                          lager:info("Disconnecting from ~p", [Name]),
                          connman:disconnect(wifi, Name)
                  end, OnlineWifiPaths),
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
handle_info({changed_led_match, Tuples}, State=#state{}) ->
    lager:info("Got LED match attempt: ~p", [Tuples]),
    {noreply, State};

handle_info({changed_qr_code, Map}, State=#state{}) ->
    gateway_config_worker:handle_qr_code(Map),
    {noreply, State};
handle_info(Msg, State) ->
    lager:warning("Unhandled info ~p ~p",[Msg, State]),
    {noreply, State}.
