-module(gateway_config).

-include("gateway_config.hrl").

-export([firmware_version/0,
         mac_address/1,
         serial_number/0,
         gps_info/0, gps_sat_info/0,
         gps_offline_assistance/1, gps_online_assistance/1,
         download_info/0, download_info/1,
         wifi_services/0, wifi_services_online/0,
         ethernet_online/0,
         advertising_enable/1, advertising_info/0,
         lights_event/1, lights_info/0,
         diagnostics/1,
         ble_device_info/0]).

firmware_version() ->
    case file:read_file("/etc/lsb_release") of
        {error,_}  ->
            Result = string:trim(os:cmd("lsb_release -rs")),
            case lists:suffix("not found", Result) of
                true ->
                    lager:warning("No firmware version found"),
                    "unknown";
                false ->
                    Result
            end;
        {ok, File} ->
            Lines = string:split(binary_to_list(File), "\n", all),
            Props = [{K, V} || [K, V] <-  [string:split(E, "=") || E <- Lines]],
            case lists:keyfind("DISTRIB_RELEASE", 1, Props) of
                false -> "unknown";
                {_, Version} -> Version
            end
    end.

mac_address(wifi) ->
    mac_address(["wlan", "wlp"]);
mac_address(eth) ->
    mac_address(["eth", "en"]);
mac_address(DevicePrefixes) when is_list(DevicePrefixes) ->
    {ok, S} = inet:getifaddrs(),
    case lists:filter(fun({K, _}) ->
                              lists:any(fun(Prefix) ->
                                                lists:prefix(Prefix, K)
                                        end, DevicePrefixes)
                      end, S) of
        [] ->
            lager:warning("No ethernet interface found"),
            "unknown";
        [{_, Props} | _] ->
            case lists:keyfind(hwaddr, 1, Props) of
                false -> "unknown";
                {_,  Addr} ->
                    lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- Addr])
            end
    end.

serial_number() ->
    mac_address(wifi).

wifi_services() ->
    %% Fetch name and strength of currently visible wifi services
    Services = lists:filtermap(fun({_Path, #{"Type" := "wifi", "Name" := Name, "Strength" := Strength}}) ->
                                       {true, {Name, Strength}};
                                  ({_Path, _}) -> false
                         end, connman:services()),
    %% Sort by signal strength
    lists:reverse(lists:keysort(2, Services)).

%% Find all services that are online or ready. There's likely only
%% ever one of these but this is how we find the target service if
%% we're connected.
-spec wifi_services_online() -> [{string(), ebus:object_path()}].
wifi_services_online() ->
    lists:filtermap(fun({Path, M}) ->
                            case maps:get("Type", M, false) == "wifi"
                                andalso lists:member(maps:get("State", M, false), ["online", "ready"]) of
                                true -> {true, {maps:get("Name", M), Path}};
                                false -> false
                            end
                    end, connman:services()).

%% Is any ethernet service online?
-spec ethernet_online() -> boolean().
ethernet_online() ->
    lists:any(fun({_Path, M}) ->
                            case maps:get("Type", M, false) == "ethernet"
                                andalso lists:member(maps:get("State", M, false), ["online"]) of
                                true -> true;
                                false -> false
                            end
                    end, connman:services()).

gps_info() ->
    gateway_config_worker:gps_info().

gps_sat_info() ->
    gateway_config_worker:gps_sat_info().

gps_offline_assistance(Path) ->
    gateway_config_worker:gps_offline_assistance(Path).

gps_online_assistance(Path) ->
    gateway_config_worker:gps_online_assistance(Path).

download_info(Value) when is_boolean(Value) ->
    gateway_config_worker:download_info(Value).

download_info() ->
    gateway_config_worker:download_info().

advertising_enable(Enable) ->
    gateway_config_worker:advertising_enable(Enable).

advertising_info() ->
    gateway_config_worker:advertising_info().

ble_device_info() ->
    gateway_config_worker:ble_device_info().

lights_event(Event) ->
    gateway_config_led:lights_event(Event).

lights_info() ->
    gateway_config_led:lights_info().


%% @doc Fetches the current diagnostics information. This includes
%% getting p2p status from a given ebus miner proxy object. The
%% diagnostics proplist will contain a list of keyed string entries to
%% indicate what the current status is on the local machine.
-spec diagnostics(ebus:proxy()) -> [{string(), string()}].
diagnostics(Proxy) ->
    Base = [{"eth",  ?MODULE:mac_address(eth)},
            {"wifi", ?MODULE:mac_address(wifi)},
            {"fw",   ?MODULE:firmware_version()}],
    P2PStatus = case ebus_proxy:call(Proxy, ?MINER_OBJECT(?MINER_MEMBER_P2P_STATUS)) of
                    {ok, [Result]} -> Result;
                    {error, "org.freedesktop.DBus.Error.ServiceUnknown"} ->
                        lager:info("Miner not ready to get p2p status"),
                        [];
                    {error, Error} ->
                        lager:notice("Failed to get p2p status: ~p", [Error]),
                        []
                end,
    %% Merge p2p status into the base
    lists:foldl(fun({Key, Val}, Acc) ->
                        lists:keystore(Key, 1, Acc, {Key, Val})
                end, Base, P2PStatus).
