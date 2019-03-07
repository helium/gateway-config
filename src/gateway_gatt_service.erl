-module(gateway_gatt_service).
-include("gateway_gatt.hrl").
-include("gateway_config.hrl").

-behavior(gatt_service).

-define(CONNMAN_AGENT_RETRY, 5000).

-export([init/1, uuid/0, handle_info/2]).

-record(state, {
                ssid="" :: string()
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
         {gateway_gatt_char_wifi_status, 0, []},
         {gateway_gatt_char_wifi_ssid, 1, []},
         {gateway_gatt_char_wifi_pass, 2, []},
         {gatt_characteristic_string, 3, [{uuid, ?UUID_GATEWAY_GATT_CHAR_MAC},
                                          {value, gateway_config:serial_number()}]},
         {gateway_gatt_char_wifi_services, 4, []},
         {gateway_gatt_char_pubkey, 5, [MinerProxy]},
         {gateway_gatt_char_add_gateway, 6, [MinerProxy]},
         {gateway_gatt_char_assert_loc, 7, [MinerProxy]},
         {gateway_gatt_char_lights, 8, []}
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
handle_info({changed_wifi_pass, Value}=Msg, State=#state{}) ->
    %% To aid the gatt online notifications we fetch all services that
    %% are wifi and online or ready and attempt to disconnect them
    %% before we try to connect to the SSID stored in the state.
    OnlineWifiPaths =
        %% Find all services that are online or ready and disconnect
        %% from them. There's likely only ever one of these but this
        %% is how we find the target service if we're connected.
        lists:filtermap(fun({_Path, M}) ->
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
    %% Start the connman agent if it was not already started.On
    %% failure to start the agent we try to restart it later and
    %% re-attempt the connect.
    case connman:start_agent() of
        ok ->
            case connman:connect(wifi, State#state.ssid, binary_to_list(Value), self()) of
                ok -> ok;
                Other ->
                    lager:notice("Start connect for SSID ~p: ~p", [State#state.ssid, Other])
            end,
            {noreply, State};
        {error, Error} ->
            lager:warning("Failed to start connman agent; ~p", [Error]),
            erlang:send_after(?CONNMAN_AGENT_RETRY, self(), Msg),
            {noreply, State}
    end;
handle_info({connect_result, _Tech, Result}, State=#state{}) ->
    lager:info("Connect result ~p", [Result]),
    {noreply, State};
handle_info({lights, Enable}, State=#state{}) ->
    gateway_config:lights_enable(Enable),
    {noreply, State};

handle_info(Msg, State) ->
    lager:warning("Unhandled info ~p ~p",[Msg, State]),
    {noreply, State}.
