-module(gateway_config_cli_registry).

-define(CLI_MODULES, [
                      gateway_config_cli_gps,
                      gateway_config_cli_download,
                      gateway_config_cli_advertise,
                      gateway_config_cli_lights,
                      gateway_config_cli_wifi
                     ]).

-export([register_cli/0, command/1]).

register_cli() ->
    clique:register(?CLI_MODULES).

command(Cmd) ->
    clique:run(Cmd).
