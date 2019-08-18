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
                   lights_event_usage()
                  ]).

register_all_cmds() ->
    lists:foreach(fun(Cmds) ->
                          [apply(clique, register_command, Cmd) || Cmd <- Cmds]
                  end,
                  [
                   lights_cmd(),
                   lights_status_cmd(),
                   lights_event_cmd()
                  ]).

%%
%% lights
%%

lights_usage() ->
    [["lights"],
     ["lights commands\n\n",
      "  status - Get the current lights status.\n"
      "  event  - Send a light event.\n"
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
%% lights event
%%

lights_event_cmd() ->
    [
     [["lights", "event", '*'], [], [], fun lights_event/3]
    ].

lights_event_usage() ->
    [["lights", "event"],
     ["lights event \n\n",
      "  panic - Set panic mode for light.\n"
      "  enable - Enable the light.\n"
      "  disable - Disable the light.\n"
     ]
    ].

lights_event(["lights", "event", EventStr], [], []) ->
    gateway_config:lights_event(list_to_atom(EventStr)),
    [clique_status:text(EventStr)];
lights_event([_, _, _], [], []) ->
    usage.
