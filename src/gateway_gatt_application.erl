-module(gateway_gatt_application).

-behavior(gatt_application).

-include("gateway_config.hrl").

-export([bus/0, adapter_path/0, path/0, init/1]).

-record(state, {
               }).

init([]) ->
    DeviceInfo = #{
                   manufacturer_name => <<"Helium">>,
                   firmware_revision => gateway_config:firmware_version(),
                   serial_number => gateway_config:serial_number()
                  },
    Services = [
                {gateway_gatt_service, 0, true},
                {gatt_service_device_information, 1, true, [DeviceInfo]}
               ],
    {ok, Services, #state{}}.

bus() ->
    ebus:system().

adapter_path() ->
    ?CONFIG_BLE_ADAPTER_PATH.

path() ->
    "/com/helium/config".
