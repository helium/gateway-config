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
    UBXArgs = application:get_all_env(ubx),
    SupFlags = {one_for_all, 3, 10},
    ChildSpecs = [
                  #{
                    id => gateway_config_worker,
                    start => {gateway_config_worker, start_link, [B, UBXArgs]},
                    type => worker,
                    restart => permanent
                  }
                 ],
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
