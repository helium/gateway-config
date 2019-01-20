-module(gateway_gatt_char_wifi_services).
-include("gateway_gatt.hrl").

-behavior(gatt_characteristic).

-export([init/2, uuid/1, flags/1,
         read_value/1]).

-record(state, { path :: ebus:object_path()
               }).


uuid(_) ->
    ?UUID_GATEWAY_GATT_CHAR_WIFI_SERVICES.

flags(_) ->
    [read].

init(Path, _) ->
    %% Currently online status is only determined by the wifi state
    Descriptors =
        [
         {gatt_descriptor_cud, 0, ["WiFi Services"]},
         {gatt_descriptor_pf, 1, [utf8_string]}
        ],
    {ok, Descriptors, #state{path=Path}}.

read_value(State=#state{}) ->
    %% Fetch name and strength of currently visible wifi services
    Services = lists:filtermap(fun({_Path, #{"Type" := "wifi", "Name" := Name, "Strength" := Strength}}) ->
                                       {true, {Name, Strength}};
                                  ({_Path, _}) -> false
                         end, connman:services()),
    %% Sort with strongest service first
    SortedServices = lists:reverse(lists:keysort(2, Services)),
    %% encode only the names and send back
    EncodedNames = jsx:encode([N || {N, _} <- SortedServices]),
    {ok, EncodedNames, State}.
