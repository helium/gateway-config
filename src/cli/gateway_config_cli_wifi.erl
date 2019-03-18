-module(gateway_config_cli_wifi).

-behavior(clique_handler).

-export([register_cli/0]).

register_cli() ->
    register_all_usage(),
    register_all_cmds().


register_all_usage() ->
    lists:foreach(fun(Args) ->
                          apply(clique, register_usage, Args)
                  end,
                  [
                   wifi_usage(),
                   wifi_services_usage(),
                   wifi_online_usage()
                  ]).

register_all_cmds() ->
    lists:foreach(fun(Cmds) ->
                          [apply(clique, register_command, Cmd) || Cmd <- Cmds]
                  end,
                  [
                   wifi_cmd(),
                   wifi_services_cmd(),
                   wifi_online_cmd()
                  ]).

%%
%% gps
%%

wifi_usage() ->
    [["wifi"],
     ["wifi commands\n\n",
      "  services - Lists available wifi services.\n"
     ]
    ].

wifi_cmd() ->
    [
     [["wifi"], [], [], fun(_, _, _) -> usage end]
    ].


%%
%% wifi services
%%

wifi_services_cmd() ->
    [
     [["wifi", "services"], [],
      [
       {gatt, [{shortname, "g"}, {longname, "gatt"}]}
      ], fun wifi_services/3]
    ].

wifi_services_usage() ->
    [["wifi", "services"],
     ["wifi services \n\n",
      "  Get the current list of available wifi services\n",
      "  --gatt\n",
      "  Limit the list as exposed over BLE\n"
     ]
    ].

wifi_services(["wifi", "services"], [], Flags) ->
    AllServices = gateway_config:wifi_services(),
    Services = case proplists:get_value(gatt, Flags, false) of
                   false ->
                       AllServices;
                   _ ->
                       {ok, S, _} = gateway_gatt_char_wifi_services:encode_services(AllServices),
                       S
               end,
    FormatService = fun({Name, Strength}) ->
                            [{name, Name}, {strength, Strength}]
                    end,
    [clique_status:table([FormatService(S) || S <- Services])];
wifi_services([_, _, _], [], []) ->
    usage.

%%
%% wifi online
%%

wifi_online_cmd() ->
    [
     [["wifi", "online"], [],
      [],
      fun wifi_online/3]
    ].

wifi_online_usage() ->
    [["wifi", "online"],
     ["wifi online \n\n",
      "  Get the current online or ready wifi servics\n"
     ]
    ].

wifi_online(["wifi", "online"], [], []) ->
    Services = gateway_config:wifi_services_online(),
    FormatService = fun({Name, Path}) ->
                               [{name, Name}, {path, Path}]
                       end,
    [clique_status:table([FormatService(S) || S <- Services])];
wifi_online([_, _], [], []) ->
    usage.
