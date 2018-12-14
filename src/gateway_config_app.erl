%%%-------------------------------------------------------------------
%% @doc gateway-config public API
%% @end
%%%-------------------------------------------------------------------

-module(gateway_config_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    gatt:start_application(gateway_gatt_application),
    case gateway_config_sup:start_link() of
        {ok, Pid} ->
            gateway_config_cli_registry:register_cli(),
            {ok, Pid};
         Other -> Other
    end.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
