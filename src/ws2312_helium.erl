-module(ws2312_helium).
-behavior(gen_server).

-export([start_link/0]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2
        ]).
-export([
         panic/0,
         disable/0,
         online/0,
         offline/0,
         undef/0,
         advert/0
]).
-define(COLOR_PANIC, <<16#00FF00:24>>).
-define(COLOR_ONLINE, <<16#FF0000:24>>).
-define(COLOR_OFFLINE, <<16#00AAAA:24>>).
-define(COLOR_UNDEFINED, <<16#00AAAA:24>>).
-define(COLOR_ADVERT, <<16#0000AA:24>>).


-type ws2312_handle() :: pid() | undefined.

-record(state, 
{
 ws2312_handle :: ws2312_handle()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    %% Trap exits to allow for terminate led animation
    erlang:process_flag(trap_exit, true),

    {ok, Handle} = ws2312:start_link(),

    {ok, #state {
            ws2312_handle = Handle
           }}.

panic() ->
    gen_server:cast(?MODULE, panic).
disable() ->
    gen_server:cast(?MODULE, disable).
online() ->
    gen_server:cast(?MODULE, online).
offline() ->
    gen_server:cast(?MODULE, offline).
undef() ->
    gen_server:cast(?MODULE, undefined).
advert() ->
    gen_server:cast(?MODULE, {advert, []}).


handle_call(Msg, _From, State = #state{}) ->
    lager:warning("Unhandled call ~p: ~p", [Msg, State]),
    {noreply, State}.

handle_cast(panic, State) ->
    ws2312:blink(?COLOR_PANIC),
    {noreply, State};
handle_cast(disable, State) ->
    ws2312:off(),
    {noreply, State};
handle_cast(online, State) ->
    ws2312:always_on(?COLOR_ONLINE),
    {noreply, State};
handle_cast(offline, State) ->
    ws2312:blink(?COLOR_OFFLINE),
    {noreply, State};
handle_cast(undefined, State) ->
    ws2312:blink(?COLOR_UNDEFINED),
    {noreply, State};
handle_cast({advert, _}, State) ->
    ws2312:blink(?COLOR_ADVERT),
    {noreply, State};
handle_cast(Msg, State) ->
    lager:info("Unhandled cast ~p: ~p", [Msg, State]),
    {noreply, State}.

handle_info(Msg, State) ->
    lager:warning("Unhandled info ~p: ~p", [Msg, State]),
    {noreply, State}.

