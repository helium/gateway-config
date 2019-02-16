-module(gateway_config_cli_lights).

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
                   lights_usage(),
                   lights_status_usage(),
                   lights_on_usage(),
                   lights_off_usage()
                  ]).

register_all_cmds() ->
    lists:foreach(fun(Cmds) ->
                          [apply(clique, register_command, Cmd) || Cmd <- Cmds]
                  end,
                  [
                   lights_cmd(),
                   lights_status_cmd(),
                   lights_on_cmd(),
                   lights_off_cmd()
                  ]).

%%
%% lights
%%

lights_usage() ->
    [["lights"],
     ["lights commands\n\n",
      "  status - Get the current lights status.\n"
      "  on     - Turn on lights.\n"
      "  off    - Turn off lights.\n"
     ]
    ].

lights_cmd() ->
    [
     [["lights"], [], [], fun(_, _, _) -> usage end]
    ].


%%
%% lights status
%%

lights_status_cmd() ->
    [
     [["lights", "status"], [], [], fun lights_status/3]
    ].

lights_status_usage() ->
    [["lights", "status"],
     ["lights status \n\n",
      "  Get the lights status.\n\n"
     ]
    ].

lights_status(["lights", "status"], [], []) ->
    Status = io_lib:format("~p", [gateway_config:lights_info()]),
    [clique_status:text(Status)];
lights_status([_, _, _], [], []) ->
    usage.


%%
%% lights on
%%

lights_on_cmd() ->
    [
     [["lights", "on"], [], [], fun lights_on/3]
    ].

lights_on_usage() ->
    [["lights", "on"],
     ["lights on \n\n",
      "  Turn on lights.\n\n"
     ]
    ].

lights_on(["lights", "on"], [], []) ->
    gateway_config:lights_enable(true),
    [clique_status:text("ok")];
lights_on([_, _, _], [], []) ->
    usage.

%%
%% lights off
%%

lights_off_cmd() ->
    [
     [["lights", "off"], [], [], fun lights_off/3]
    ].

lights_off_usage() ->
    [["lights", "off"],
     ["lights off \n\n",
      "  Turn off lights.\n\n"
     ]
    ].

lights_off(["lights", "off"], [], []) ->
    gateway_config:lights_enable(false),
    [clique_status:text("ok")];
lights_off([_, _, _], [], []) ->
    usage.
