-module(gateway_config_cli_advertise).

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
                   advertise_usage(),
                   advertise_status_usage(),
                   advertise_on_usage(),
                   advertise_off_usage()
                  ]).

register_all_cmds() ->
    lists:foreach(fun(Cmds) ->
                          [apply(clique, register_command, Cmd) || Cmd <- Cmds]
                  end,
                  [
                   advertise_cmd(),
                   advertise_status_cmd(),
                   advertise_on_cmd(),
                   advertise_off_cmd()
                  ]).

%%
%% advertise
%%

advertise_usage() ->
    [["advertise"],
     ["advertise commands\n\n",
      "  status - Get the current ble advertisement status.\n"
      "  on     - Tuen on advertising.\n"
      "  off    - Tuen off advertising.\n"
     ]
    ].

advertise_cmd() ->
    [
     [["advertise"], [], [], fun(_, _, _) -> usage end]
    ].


%%
%% advertise status
%%

advertise_status_cmd() ->
    [
     [["advertise", "status"], [], [], fun advertise_status/3]
    ].

advertise_status_usage() ->
    [["advertise", "status"],
     ["advertise status \n\n",
      "  Get the advertise status.\n\n"
     ]
    ].

advertise_status(["advertise", "status"], [], []) ->
    Status = io_lib:format("~p", [gateway_config:advertising_info()]),
    [clique_status:text(Status)];
advertise_status([_, _, _], [], []) ->
    usage.


%%
%% advertise on
%%

advertise_on_cmd() ->
    [
     [["advertise", "on"], [], [], fun advertise_on/3]
    ].

advertise_on_usage() ->
    [["advertise", "on"],
     ["advertise on \n\n",
      "  Turn on advertising.\n\n"
     ]
    ].

advertise_on(["advertise", "on"], [], []) ->
    gateway_config:advertising_enable(true),
    [clique_status:text("ok")];
advertise_on([_, _, _], [], []) ->
    usage.

%%
%% advertise off
%%

advertise_off_cmd() ->
    [
     [["advertise", "off"], [], [], fun advertise_off/3]
    ].

advertise_off_usage() ->
    [["advertise", "off"],
     ["advertise off \n\n",
      "  Turn off advertising.\n\n"
     ]
    ].

advertise_off(["advertise", "off"], [], []) ->
    gateway_config:advertising_enable(false),
    [clique_status:text("ok")];
advertise_off([_, _, _], [], []) ->
    usage.
