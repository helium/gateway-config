-module(gateway_gatt_service).
-include("gateway_gatt.hrl").

-behavior(gatt_service).

-export([init/1, uuid/1]).

uuid(_) ->
    ?UUID_GATEWAY_GATT_SERVICE.

init(_) ->
    %% TODO: Connman crashing invalidates a number of pids that are
    %% used in characteristics. We should probably monitor and
    %% restart
    Characteristics =
        [
         {gateway_gatt_char_wifi_status, 0, []},
         {gateway_gatt_char_wifi_ssid, 1, []},
         {gateway_gatt_char_wifi_pass, 2, []}
        ],
    {ok, Characteristics, []}.
