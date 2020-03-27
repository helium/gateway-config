-module(gateway_gatt_service).
-include("gateway_gatt.hrl").
-include("gateway_config.hrl").

-behavior(gatt_service).

-define(CONNMAN_AGENT_RETRY, 5000).
-define(ENABLE_WIFI_RETRY, 10000).

-export([init/1, uuid/0, handle_info/2]).

-record(state, {
                connect_result_char=undefined :: undefined | ebus:object_path()
               }).

uuid() ->
    ?UUID_GATEWAY_GATT_SERVICE.

init(_) ->
    {ok, Bus} = ebus:system(),
    {ok, MinerProxy} = ebus_proxy:start_link(Bus, ?MINER_APPLICATION_NAME, []),

    %% TODO: Connman crashing invalidates a number of pids that are
    %% used in characteristics. We should probably monitor and
    %% restart
    Characteristics =
        [
         {gateway_gatt_char_wifi_connect, 1, []},
         {gatt_characteristic_string, 2, [{uuid, ?UUID_GATEWAY_GATT_CHAR_MAC},
                                          {value, gateway_config:serial_number()}]},
         {gateway_gatt_char_wifi_ssid, 3, []},
         {gateway_gatt_char_wifi_services, 4, []},
         {gateway_gatt_char_pubkey, 5, [MinerProxy]},
         {gateway_gatt_char_add_gateway, 6, [MinerProxy]},
         {gateway_gatt_char_assert_loc, 7, [MinerProxy]},
         {gateway_gatt_char_lights, 8, []},
         {gateway_gatt_char_onboarding_key, 9, [MinerProxy]},
         {gateway_gatt_char_diagnostics, 10, [MinerProxy]},
         {gateway_gatt_char_eth_online, 11, []},
         {gateway_gatt_char_wifi_remove, 12, []},
         {gateway_gatt_char_wifi_credentials, 13, []}
        ],
    self() ! enable_wifi,
    {ok, Characteristics, #state{}}.

handle_info(enable_wifi, State=#state{}) ->
    connman:enable(wifi, true),
    erlang:send_after(?ENABLE_WIFI_RETRY, self(), ensure_wifi),
    {noreply, State};
handle_info(ensure_wifi, State=#state{}) ->
    case connman:state({tech, wifi}) of
        {error, Error} ->
            lager:warning("Failed to get Wi-Fi state; Re-enabling Wi-Fi: ~p", [Error]),
            self() ! enable_wifi,
            {noreply, State};
        {ok, disabled} ->
            lager:warning("Failed to enable Wi-Fi; Retrying", []),
            self() ! enable_wifi,
            {noreply, State};
        {ok, _} ->
            {noreply, State}
    end;
handle_info({connect, wifi, _, _, Char}, State=#state{connect_result_char=E}) when E =/= undefined ->
    self() ! {ebus_info, Char, {error, already_connecting}},
    {noreply, State};
handle_info({connect, wifi, Service, Pass, Char}=Msg, State=#state{}) ->
    lager:info("Trying to connect to WiFi SSID: ~p", [Service]),
    %% Start the connman agent if it was not already started. On
    %% failure to start the agent we try to restart it later and
    %% re-attempt the connect.
    case connman:start_agent() of
        ok ->
            case connman:connect(wifi, Service, Pass, self()) of
                ok ->
                    {noreply, State#state{connect_result_char=Char}};
                Other ->
                    lager:notice("Error connecting connman to SSID ~p: ~p", [Service, Other]),
                    {noreply, State}
            end;
        {error, Error} ->
            lager:warning("Failed to start connman agent; ~p", [Error]),
            erlang:send_after(?CONNMAN_AGENT_RETRY, self(), Msg),
            {noreply, State}
    end;
handle_info({connect_result, _Tech, Result}=Msg, State=#state{}) ->
    lager:info("Connect result ~p", [Result]),
    self() ! {ebus_info, State#state.connect_result_char, Msg},
    {noreply, State#state{connect_result_char=undefined}};
handle_info({remove, wifi, Service, _Char}=Msg, State=#state{}) ->
    lager:info("Trying to remove WiFi SSID: ~p", [Service]),
    %% Start the connman agent if it was not already started. On
    %% failure to start the agent we try to restart it later and
    %% re-attempt the remove.
    case connman:start_agent() of
        ok ->
            case connman:remove(wifi, Service) of
                ok ->
                    {noreply, State};
                Other ->
                    lager:notice("Error removing WiFi SSID ~p: ~p", [Service, Other]),
                    {noreply, State}
            end;
        {error, Error} ->
            lager:warning("Failed to start connman agent; ~p", [Error]),
            erlang:send_after(?CONNMAN_AGENT_RETRY, self(), Msg),
            {noreply, State}
    end;
handle_info({lights, Enable}, State=#state{}) ->
    Event = case Enable of
                true -> enable;
                _ -> disable
            end,
    gateway_config:lights_event(Event),
    {noreply, State};

handle_info(Msg, State) ->
    lager:warning("Unhandled info ~p ~p",[Msg, State]),
    {noreply, State}.
