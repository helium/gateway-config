-module(gateway_config_miner).

-behavior(gen_server).

-include("gateway_config.hrl").
-define(SERVICE, 'helium.local.api').

%% gen_server
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

%% api
-export([
    status/0,
    pubkey/0,
    add_gateway/3
]).

-record(state, {
    connection = undefined :: grpc_client:connection() | undefined
}).

-spec status() -> [tuple()].
status() ->
    gen_server:call(?MODULE, status).

-spec pubkey() -> {PubKey :: binary(), OnboardingKey :: binary()}.
pubkey() ->
    gen_server:call(?MODULE, pubkey).

-spec add_gateway(
    Owner :: libp2p_crypto:pubkey_bin(),
    Payer :: libp2p_crypto:pubkey_bin(),
    Mode :: full | dataonly | light
) -> {ok, binary()} | {error, term()}.
add_gateway(Owner, Payer, Mode) ->
    gen_server:call(?MODULE, {add_gateway, [Owner, Payer, Mode]}).

init(_) ->
    erlang:register(?MODULE, self()),
    {ok, #state{}}.

handle_call(Call, From, State = #state{connection = undefined}) ->
    case grpc_client:connect(tcp, "localhost", 4467) of
        {ok, Connection} ->
            handle_call(Call, From, State#state{connection = Connection});
        {error, Error} ->
            {reply, {error, Error}, State}
    end;
handle_call(status, _From, State = #state{connection = Connection}) ->
    case call_unary(Connection, height, #{}) of
        {ok, Result} ->
            Height = kvc:path(<<"result.height">>, Result),
            {reply, {ok, [{"connected", "yes"}, {"height", integer_to_list(Height)}]}, State};
        {error, _} ->
            {reply, {ok, [{"connected", "no"}]}, State#state{connection = undefined}}
    end;
handle_call(pubkey, _From, State = #state{connection = Connection}) ->
    case call_unary(Connection, pubkey, #{}) of
        {ok, Result} ->
            PubKey = kvc:path(<<"result.address">>, Result),
            OnboardingKey = kvc:path(<<"result.onboarding_address">>, Result),
            {reply, {ok, {PubKey, OnboardingKey}}, State};
        {error, _} ->
            {reply, {ok, [{"connected", "no"}]}, State#state{connection = undefined}}
    end;
handle_call({add_gateway, [Owner, Payer, Mode]}, _From, State = #state{connection = Connection}) ->
    case
        call_unary(Connection, add_gateway, #{
            owner => Owner,
            payer => Payer,
            staking_mode => Mode
        })
    of
        {ok, Result} ->
            BinTxn = kvc:path(<<"result.add_gateway_txn">>, Result),
            {reply, {ok, BinTxn}, State};
        {error, Error} ->
            {repl, {error, Error}, State}
    end;
handle_call(Msg, _From, State = #state{}) ->
    lager:warning("Unhandled call ~p", [Msg]),
    {reply, ok, State}.

handle_cast(Msg, State = #state{}) ->
    lager:warning("Unhandled cast ~p", [Msg]),
    {noreply, State}.

handle_info(_Msg, State) ->
    lager:warning("unhandled info message ~p", [_Msg]),
    {noreply, State}.

call_unary(Connection, Method, Arguments) ->
    grpc_client:unary(Connection, Arguments, ?SERVICE, Method, gateway_local_client_pb, []).
