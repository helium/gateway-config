-module(gateway_gatt_char_qr_code).
-include("gateway_gatt.hrl").

-behavior(gatt_characteristic).

-export([init/2, uuid/1, flags/1,
         write_value/2]).

-record(state, { path :: ebus:object_path(),
                 value :: #{binary() => binary()}
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
     #state{path=Path, value=#{}}}.


write_value(State=#state{}, Bin) ->
    lager:info("Changed QR code"),
    %% Catch errors in case there is a malformed binary
    case (catch jsx:decode(Bin)) of
        {'EXIT', Reason} ->
            lager:warning("Failed to parse QRCode JSON: ~p", [Reason]),
            {ok, State};
        BinEntries ->
            Entries = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- BinEntries],
            lager:info("Decoded QRCode JSON: ~p", [Entries]),
            Map = maps:from_list(Entries),
            self() ! {changed_qr_code, Map},
            {ok, State#state{value=Map}}
    end.
