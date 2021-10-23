-module(gateway_ble_advertisement).

-behavior(ble_advertisement).

-export([
    adapter_path/0,
    type/0,
    init/1,
    services/1,
    include_tx_power/1,
    local_name/1
]).

adapter_path() ->
    gateway_gatt_application:adapter_path().

type() ->
    peripheral.

init(_) ->
    {ok, []}.

services(_) ->
    [gateway_gatt_service].

include_tx_power(_) ->
    true.

local_name(_) ->
    Serial = gateway_config:serial_number(),
    "Helium Hotspot " ++ lists:nthtail(length(Serial) - 4, Serial).
