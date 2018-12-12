-module(gateway_config).

-export([firmware_version/0, serial_number/0]).

firmware_version() ->
    case file:read_file("/etc/lsb_release") of
        {error,_}  ->
            string:trim(os:cmd("lsb_release -rs"));
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
    [{_, Props} | _] = lists:filter(fun({K, _}) ->
                                            lists:prefix("eth", K) orelse
                                                lists:prefix("en", K)
                                    end, S),
    case lists:keyfind(hwaddr, 1, Props) of
        false -> "unknown";
        {_,  Addr} ->
             string:join([io_lib:format("~2.16.0B", [X]) || X <- Addr], ":")
    end.
