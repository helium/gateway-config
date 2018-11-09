-module(gateway_gatt_application).

-behavior(gatt_application).

-export([bus/0, adapter_path/0, path/0, init/1]).

init([]) ->
    Services = [
                {gateway_gatt_service, 0, true}
               ],
    {ok, Services, []}.

bus() ->
    ebus:system().

adapter_path() ->
    "/org/bluez/hci0".

path() ->
    "/com/helium/config".
