-module(gateway_config).

-export([firmware_version/0, serial_number/0,
         gps_info/0, gps_sat_info/0, gps_offline_assistance/1, gps_online_assistance/1,
         download_info/0, download_info/1,
         wifi_services/0,
         advertising_enable/1, advertising_info/0,
         lights_enable/1, lights_info/0]).

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

serial_number() ->
    {ok, S} = inet:getifaddrs(),
    case lists:filter(fun({K, _}) ->
                              lists:prefix("eth", K) orelse
                                  lists:prefix("en", K)
                      end, S) of
        [] ->
            lager:warning("No ethernet interface found"),
            "unknown";
        [{_, Props} | _] ->
            case lists:keyfind(hwaddr, 1, Props) of
                false -> "unknown";
                {_,  Addr} ->
                    string:join([io_lib:format("~2.16.0B", [X]) || X <- Addr], ":")
            end
    end.

wifi_services() ->
    %% Fetch name and strength of currently visible wifi services
    Services = lists:filtermap(fun({_Path, #{"Type" := "wifi", "Name" := Name, "Strength" := Strength}}) ->
                                       {true, {Name, Strength}};
                                  ({_Path, _}) -> false
                         end, connman:services()),
    %% Sort by signal strength
    lists:reverse(lists:keysort(2, Services)).

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

lights_enable(Enable) ->
    gateway_config_worker:lights_enable(Enable).

lights_info() ->
    gateway_config_worker:lights_info().
