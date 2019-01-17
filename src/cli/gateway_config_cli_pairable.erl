-module(gateway_config_cli_pairable).

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
                   pairable_usage(),
                   pairable_status_usage(),
                   pairable_on_usage(),
                   pairable_off_usage()
                  ]).

register_all_cmds() ->
    lists:foreach(fun(Cmds) ->
                          [apply(clique, register_command, Cmd) || Cmd <- Cmds]
                  end,
                  [
                   pairable_cmd(),
                   pairable_status_cmd(),
                   pairable_on_cmd(),
                   pairable_off_cmd()
                  ]).

%%
%% pairable
%%

pairable_usage() ->
    [["pairable"],
     ["pairable commands\n\n",
      "  status - Get the current pairing status.\n"
      "  on     - Tuen on pairing.\n"
      "  off    - Tuen off pairing.\n"
     ]
    ].

pairable_cmd() ->
    [
     [["pairable"], [], [], fun(_, _, _) -> usage end]
    ].


%%
%% pairable status
%%

pairable_status_cmd() ->
    [
     [["pairable", "status"], [], [], fun pairable_status/3]
    ].

pairable_status_usage() ->
    [["pairable", "status"],
     ["pairable status \n\n",
      "  Get the pairable status.\n\n"
     ]
    ].

pairable_status(["pairable", "status"], [], []) ->
    Stats = gateway_config:pairing_info(),
    Rows = lists:map(fun({Key, Val}) ->
                             [{key, Key}, {value, Val}]
                     end, Stats),
    [clique_status:table(Rows)];
pairable_status([_, _, _], [], []) ->
    usage.


%%
%% pairable on
%%

pairable_on_cmd() ->
    [
     [["pairable", "on"], [], [], fun pairable_on/3]
    ].

pairable_on_usage() ->
    [["pairable", "on"],
     ["pairable on \n\n",
      "  Turn on pairability.\n\n"
     ]
    ].

pairable_on(["pairable", "on"], [], []) ->
    gateway_config:pairing_enable(true),
    [clique_status:text("ok")];
pairable_on([_, _, _], [], []) ->
    usage.

%%
%% pairable off
%%

pairable_off_cmd() ->
    [
     [["pairable", "off"], [], [], fun pairable_off/3]
    ].

pairable_off_usage() ->
    [["pairable", "off"],
     ["pairable off \n\n",
      "  Turn off pairability.\n\n"
     ]
    ].

pairable_off(["pairable", "off"], [], []) ->
    gateway_config:pairing_enable(false),
    [clique_status:text("ok")];
pairable_off([_, _, _], [], []) ->
    usage.
