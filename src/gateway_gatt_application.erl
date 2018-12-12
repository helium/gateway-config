-module(gateway_gatt_application).

-behavior(gatt_application).

-export([bus/0, adapter_path/0, path/0, init/1, terminate/2]).

-record(state, {
                advertisement :: pid()
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
    {ok, Bus} = bus(),
    {ok, AdvPid} = gatt_advertisement:start_link(Bus, path(), 0, gateway_gatt_advertisement, []),
    {ok, Services, #state{advertisement=AdvPid}}.

bus() ->
    ebus:system().

adapter_path() ->
    "/org/bluez/hci0".

path() ->
    "/com/helium/config".

terminate(Reason, #state{advertisement=AdvPid}) ->
    gatt_advertisement:stop(AdvPid, Reason).
