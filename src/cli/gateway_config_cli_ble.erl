-module(gateway_config_cli_ble).

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
                   ble_usage(),
                   ble_devices_usage()
                  ]).

register_all_cmds() ->
    lists:foreach(fun(Cmds) ->
                          [apply(clique, register_command, Cmd) || Cmd <- Cmds]
                  end,
                  [
                   ble_cmd(),
                   ble_devices_cmd()
                  ]).

%%
%% ble
%%

ble_usage() ->
    [["ble"],
     ["ble commands\n\n",
      "  devices - Lists all devices known to BLE.\n"
     ]
    ].

ble_cmd() ->
    [
     [["ble"], [], [], fun(_, _, _) -> usage end]
    ].


%%
%% ble devices
%%

ble_devices_cmd() ->
    [
     [["ble", "devices"], [],
      [
       {connected, [{shortname, "c"}, {longname, "connected"}]}
      ], fun ble_devices/3]
    ].

ble_devices_usage() ->
    [["ble", "devices"],
     ["ble devices \n\n",
      "  Get the current list of devices BLE has connected with\n",
      "  --connected\n",
      "  Return true or false if any device is connected to BLE\n"
     ]
    ].

ble_devices(["ble", "devices"], [], [{connected, _}]) ->
    {ok, AllDevices} = gateway_config:ble_device_info(),
    case lists:filter(fun(#{"Connected" := true}) -> true;
                         (_) -> false
                      end, AllDevices) of
        [] -> [clique_status:text("false")];
        _ ->  [clique_status:text("true")]
    end;
ble_devices(["ble", "devices"], [], []) ->
    {ok, AllDevices} = gateway_config:ble_device_info(),
    FormatDevice = fun(#{"Name" := Name,
                         "Address" := Address,
                         "Paired" := Paired,
                         "Connected" := Connected}) ->
                           [{address, Address}, {name, Name}, {paired, Paired}, {connected, Connected}]
                    end,
    [clique_status:table([FormatDevice(D) || D <- AllDevices])].
