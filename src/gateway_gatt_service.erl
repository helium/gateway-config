-module(gateway_gatt_service).

-behavior(gatt_service).

-export([init/1, uuid/1]).

uuid(_) ->
    "12345678-1234-5678-1234-56789abcdef7".

init(_) ->
    Characteristics =
        [
         {gateway_gatt_char_wifi_status, 0, []}
        ],
    {ok, Characteristics, []}.
