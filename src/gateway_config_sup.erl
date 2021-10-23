%%%-------------------------------------------------------------------
%% @doc gateway-config top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(gateway_config_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, B} = ebus:system(),
    SupFlags = {one_for_all, 3, 10},
    ChildSpecs = [
        #{
            id => gateway_config_worker,
            start => {gateway_config_worker, start_link, [B]},
            type => worker,
            restart => permanent
        },
        #{
            id => gateway_config_led,
            start => {gateway_config_led, start_link, [B]},
            type => worker,
            restart => permanent
        },
        #{
            id => gateway_gatt_application,
            restart => permanent,
            type => supervisor,
            start => {gatt_application_sup, start_link, [gateway_gatt_application, []]}
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
