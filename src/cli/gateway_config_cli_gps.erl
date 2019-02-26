-module(gateway_config_cli_gps).

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
                   gps_usage(),
                   gps_info_usage(),
                   gps_offline_usage(),
                   gps_online_usage()
                  ]).

register_all_cmds() ->
    lists:foreach(fun(Cmds) ->
                          [apply(clique, register_command, Cmd) || Cmd <- Cmds]
                  end,
                  [
                   gps_cmd(),
                   gps_info_cmd(),
                   gps_offline_cmd(),
                   gps_online_cmd()
                  ]).

%%
%% gps
%%

gps_usage() ->
    [["gps"],
     ["gps commands\n\n",
      "  info - Get information about the current GPS state.\n"
      "  offline <path> - Load AssistNow Offline data file.\n"
      "  online <path> - Load AssistNow Online data file.\n"
     ]
    ].

gps_cmd() ->
    [
     [["gps"], [], [], fun(_, _, _) -> usage end]
    ].


%%
%% gps info
%%

gps_info_cmd() ->
    [
     [["gps", "info"], [], [], fun gps_info/3]
    ].

gps_info_usage() ->
    [["gps", "info"],
     ["gps info \n\n",
      "  Get information about the current GPS state.\n\n"
     ]
    ].

gps_info(["gps", "info"], [], []) ->
    GPSInfo = gateway_config:gps_info(),
    SatInfo = gateway_config:gps_sat_info(),
    [format_info(GPSInfo),
     format_sat_info(SatInfo)];
gps_info([_, _, _], [], []) ->
    usage.

format_info(GPSInfo) when map_size(GPSInfo) == 0 ->
    clique_status:text("No GPS Info Available");
format_info(#{fix_type := FixType, num_sats := NumSV, lat := Lat, lon := Lon,
              h_acc := HAcc, v_acc := VAcc, t_acc := TAcc}) ->
    Row = [{fix_type, ubx:fix_type(FixType)},
           {num_sats,  NumSV},
           {lat, Lat},
           {lon,  Lon},
           {h_acc, HAcc},
           {v_acc, VAcc},
           {t_acc, TAcc}],
    clique_status:table([Row]).

format_sat_info([]) ->
    clique_status:text("No GPS Satellite Info Available");
format_sat_info(SatInfos) ->
    FormatSatInfo =
        fun(#{type := Type, id := SvID, elevation := Elevation, azimuth := Azimuth,
              quality := Quality, cno := CNO, health := Health, used := Used,
              orbit := OrbitSource}) ->
                [{type, Type},
                 {sv_id, SvID},
                 {elevation, Elevation},
                 {azimuth, Azimuth},
                 {strength, CNO},
                 {health, Health},
                 {quality, Quality},
                 {orbit, OrbitSource},
                 {used, Used}
                ]
        end,
    clique_status:table(lists:map(FormatSatInfo, SatInfos)).


%%
%% gps offline
%%

gps_offline_cmd() ->
    [
     [["gps", "offline", '*'], [], [], fun gps_offline/3]
    ].

gps_offline_usage() ->
    [["gps", "offline"],
     ["gps offline </path/to/offline.ubx>\n\n",
      "  Send AssistNow Offline data file to GPS receiver.\n\n"
     ]
    ].

gps_offline(["gps", "offline", Path], [], []) ->
    case gateway_config:gps_offline_assistance(Path) of
        ok ->
            [clique_status:text("Done sending AssistNow Offline data")];
        {error, Reason} ->
            [clique_status:test("Failed to send AssistNow Offline data; Reason: ~p", [Reason])]
    end;
gps_offline(_, _, _) ->
    usage.


%%
%% gps online
%%

gps_online_cmd() ->
    [
     [["gps", "online", '*'], [], [], fun gps_online/3]
    ].

gps_online_usage() ->
    [["gps", "online"],
     ["gps online </path/to/online.ubx>\n\n",
      "  Send AssistNow Online data file to GPS receiver.\n\n"
     ]
    ].

gps_online(["gps", "online", Path], [], []) ->
    case gateway_config:gps_online_assistance(Path) of
        ok ->
            [clique_status:text("Done sending AssistNow Online data")];
        {error, Reason} ->
            [clique_status:test("Failed to send AssistNow Online data; Reason: ~p", [Reason])]
    end;
gps_online(_, _, _) ->
    usage.
