-module(gateway_config).

-include("gateway_config.hrl").

-export([firmware_version/0,
         mac_address/1,
         ip_address/0,
         serial_number/0,
         wifi_services/0, wifi_services_online/0, wifi_services_configured/0,
         wifi_services_named/1,
         ethernet_online/0,
         advertising_enable/1, advertising_info/0,
         lights_event/1, lights_info/0,
         diagnostics/1,
         get_public_key/1,
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

ip_address() ->
    {ok, Addrs} = inet:getifaddrs(),
    NonLocals = [
        Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts,
        size(Addr) == 4, Addr =/= {127,0,0,1}
    ],
    case NonLocals of
        [] ->
            "none";
        _ ->
            inet:ntoa(hd(NonLocals))
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

get_all_lines(Filename) ->
    {ok, IO} = file:open(Filename, [read]),
    Lines = get_all_lines(io:get_line(IO, ""), IO, []),
    file:close(IO),
    Lines.

get_all_lines(eof, _IO, Acc) -> lists:reverse(Acc);
get_all_lines({error, _Error}, _IO, Acc) -> lists:reverse(Acc);
get_all_lines(Line, IO, Acc) -> get_all_lines(io:get_line(IO, ""), IO, [Line | Acc]).

extract_value(Key, Lines) ->
    Prefix = Key ++ "=",
    lists:filtermap(fun(Line) ->
                        case string:str(Line, Prefix) > 0 of
                            true ->
                                Start = string:str(Line, Prefix) + string:len(Prefix),
                                Value = string:substr(Line, Start),
                                {true, string:trim(Value, trailing, "\n")};
                            false -> false
                        end
                    end, Lines).

parse_wifi_network_name_from_settings(Filename) ->
    Lines = get_all_lines(Filename),
    case lists:any(fun(Line) -> string:equal(Line, "Favorite=true\n") end, Lines) of
        true ->
            Dirname = filename:dirname(Filename),
            Service = lists:last(filename:split(Dirname)),
            Prefix = string:sub_string(Service, 1, 5),
            case string:equal(Prefix, "wifi_") of
                true ->
                    [Network] = extract_value("Name", Lines),
                    {true, Network};
                false -> false
            end;
        false -> false
    end.

parse_wifi_service_from_settings(Network, Filename) ->
    Lines = get_all_lines(Filename),
    case lists:any(fun(Line) -> string:equal(Line, "Name=" ++ Network ++ "\n") end, Lines) of
        true ->
            Dirname = filename:dirname(Filename),
            Service = lists:last(filename:split(Dirname)),
            Prefix = string:sub_string(Service, 1, 5),
            case string:equal(Prefix, "wifi_") of
                true ->
                    {true, Service};
                false -> false
            end;
        false -> false
    end.

wifi_services_configured() ->
    case file:list_dir(?CONNMAN_PROFILES_PATH) of
        {ok, Filenames} ->
            lists:filtermap(fun(Filename) ->
                                case filelib:is_dir(?CONNMAN_PROFILES_PATH ++ Filename) of
                                    true ->
                                        case filelib:find_file("settings", ?CONNMAN_PROFILES_PATH ++ Filename) of
                                            {ok, Settings} ->
                                                parse_wifi_network_name_from_settings(Settings);
                                            {error, not_found} -> false
                                        end;
                                    false -> false
                                end
                            end, Filenames);
        _ -> []
    end.

wifi_services_named(Name) ->
    case file:list_dir(?CONNMAN_PROFILES_PATH) of
        {ok, Filenames} ->
            lists:filtermap(fun(Filename) ->
                                case filelib:is_dir(?CONNMAN_PROFILES_PATH ++ Filename) of
                                    true ->
                                        case filelib:find_file("settings", ?CONNMAN_PROFILES_PATH ++ Filename) of
                                            {ok, Settings} ->
                                                parse_wifi_service_from_settings(Name, Settings);
                                            {error, not_found} -> false
                                        end;
                                    false -> false
                                end
                            end, Filenames);
        _ -> []
    end.

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
            {"fw",   ?MODULE:firmware_version()},
            {"ip",   ?MODULE:ip_address()}],
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


-spec get_public_key(onboarding_key | pubkey) -> {ok, string()} | {error, term()}.
get_public_key(KeyName) ->
    KeysFile = application:get_env(gateway_config, keys_file, "data/public_keys"),
    case file:consult(KeysFile) of
        {error, Error} -> {error, Error};
        {ok, KeyList} ->
            case proplists:get_value(KeyName, KeyList, undefined) of
                undefined ->
                    {error, key_not_found};
                V ->
                    {ok, V}
            end
    end.
