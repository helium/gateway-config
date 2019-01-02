-module(gateway_config_cli_download).

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
                   download_usage()
                  ]).

register_all_cmds() ->
    lists:foreach(fun(Cmds) ->
                          [apply(clique, register_command, Cmd) || Cmd <- Cmds]
                  end,
                  [
                   download_cmd()
                  ]).

%%
%% download
%%

download_usage() ->
    [["download"],
     ["download commands\n\n",
      "  start - Inform the configuration system of a started firmware download.\n"
      "  stop - Inform the configuration system of a stopped firmware download.\n"
      "  info - Get current cached download status from configuration system.\n"
     ]
    ].

download_cmd() ->
    [
     [["download", '*'], [], [], fun download_cmd/3]
    ].


download_cmd(["download", "start"], [], []) ->
    gateway_config:download_info(true),
    [clique_status:text("ok")];
download_cmd(["download", "stop"], [], []) ->
    gateway_config:download_info(false),
    [clique_status:text("ok")];
download_cmd(["download", "info"], [], []) ->
    case gateway_config:download_info() of
        true -> [clique_status:text("started")];
        false -> [clique_status:text("stopped")]
    end;
download_cmd(_, _, _) ->
    usage.
