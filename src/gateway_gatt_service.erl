-module(gateway_gatt_service).

-behavior(gatt_service).

-export([init/1, uuid/1]).

uuid(_) ->
    "12345678-1234-5678-1234-56789abcdef7".

init(_) ->
    %% TODO: Connman crashing invalidates a number of links here. We
    %% should probably monitor and restart?
    {ok, Connman} = connman:connman(),
    Characteristics =
        [
         {gateway_gatt_char_wifi_status, 0, [Connman]}
        ],
    {ok, Characteristics, []}.
