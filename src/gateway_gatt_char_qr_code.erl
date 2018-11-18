-module(gateway_gatt_char_qr_code).
-include("gateway_gatt.hrl").

-behavior(gatt_characteristic).

-export([init/2, uuid/1, flags/1,
         write_value/2]).

-record(state, { path :: ebus:object_path(),
                 value :: binary()
               }).


uuid(#state{}) ->
    ?UUID_GATEWAY_GATT_CHAR_QR_CODE.

flags(#state{}) ->
    [write].

init(Path, _) ->
    Descriptors =
        [
         {gatt_descriptor_cud, 0, ["QR Code"]}
        ],
    {ok, Descriptors,
     #state{path=Path, value= <<>>}}.


write_value(State=#state{}, Bin) ->
    lager:info("Changed QR code"),
    self() ! {changed_qr_code, Bin},
    {ok, State#state{value=Bin}}.
