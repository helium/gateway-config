-module(gateway_config_worker).

-behavior(ebus_object).

-define(WORKER, gateway_config).

-include("gateway_config.hrl").

%% ebus_object
-export([start_link/2, init/1, handle_info/2, handle_message/3, terminate/2]).
%% api
-export([handle_qr_code/1]).


-record(state, {
                ubx_handle :: pid() | undefined,
                gps_lock=false :: boolean(),
                gps_position=#{} :: #{string() => float()}
               }).

%% API
handle_qr_code(Map) ->
    ?WORKER ! {handle_qr_code, Map}.


%% ebus_object

start_link(Bus, Args) ->
    ok = ebus:request_name(Bus, ?CONFIG_APPLICATION_NAME),
    ebus_object:start_link(Bus, ?CONFIG_OBJECT_PATH, ?MODULE, Args, []).

init(Args) ->
    erlang:register(?WORKER, self()),
    Filename = proplists:get_value(filename, Args),
    case (file:read_file_info("/dev/"++Filename)) of
        {ok, _} ->
            Gpio = proplists:get_value(gpio, Args, 68),
            {ok, Pid} = ubx:start_link(Filename, Gpio, [], self()),
            ubx:enable_message(Pid, nav_posllh, 5),
            ubx:enable_message(Pid, nav_sol, 5),
            {ok, #state{ubx_handle=Pid}};
        _ ->
            lager:warning("No UBX filename or device found, running in stub mode"),
            {ok, #state{ubx_handle=undefined}}
    end.


handle_message(?CONFIG_MEMBER_POSITION, _Msg, State=#state{}) ->
    {reply,
     [bool, {dict, string, double}],
     [State#state.gps_lock, State#state.gps_position], State};
handle_message(Member, _Msg, State) ->
    lager:warning("Unhandled message ~p", Member),
    {noreply, State}.


handle_info({nav_sol, 3}, State=#state{gps_lock=Lock}) ->
    case Lock of
        false ->
            %% Reeceive a gps lock when we did not have a lock. Set the lock
            %% and signal out
            lager:debug("Signaling GPS locked"),
            {noreply, State#state{gps_lock=true},
             {signal, ?CONFIG_OBJECT_PATH, ?CONFIG_OBJECT_INTERFACE, ?CONFIG_MEMBER_POSITION_LOCK,
              [bool], [true]}};
        true ->
            %% Ignore a lock update if we already had a lock
            {noreply, State}
    end;
handle_info({nav_sol, _}, State=#state{gps_lock=Lock}) ->
    case Lock of
        true ->
        %% Reeceive a gps unlock when we did have a lock. Clear the lock
        %% and signal out
            lager:debug("Signaling GPS not locked"),
            {noreply, State#state{gps_lock=false},
             {signal, ?CONFIG_OBJECT_PATH, ?CONFIG_OBJECT_INTERFACE, ?CONFIG_MEMBER_POSITION_LOCK,
              [bool], [false]}};
        false ->
            %% Ignore a no lock message if we already know we're not
            %% locked
            {noreply, State}
    end;
handle_info({nav_posllh, _}, State=#state{gps_lock=false}) ->
    %% Do not signal a position if we don't have a lock
    {noreply, State};
handle_info({nav_posllh, {Lat,Lon,Height,HorizontalAcc,VerticalAcc}}, State=#state{}) ->
    Position = #{
                 "lat" => Lat,
                 "lon" => Lon,
                 "height" => Height,
                 "h_accuracy" => HorizontalAcc,
                 "v_accuracy" => VerticalAcc
                },
    lager:debug("Signaling locked GPS position ~p", [Position]),
    {noreply, State#state{gps_position=Position},
     {signal, ?CONFIG_OBJECT_PATH, ?CONFIG_OBJECT_INTERFACE, ?CONFIG_MEMBER_POSITION,
      [{dict, string, double}], [Position]}};
handle_info({packet, Packet}, State=#state{}) ->
    lager:warning("Ignoring unrequested ubx packet: ~p", [Packet]),
    {noreply, State};
handle_info({handle_qr_code, Map}, State=#state{}) ->
    lager:info("Signaling Add Gateway with: ~p", [Map]),
    {noreply, State,
     {signal, ?CONFIG_OBJECT_PATH, ?CONFIG_OBJECT_INTERFACE, ?CONFIG_MEMBER_ADD_GW,
      [{dict, string, string}], [Map]
     }
    };

handle_info(_Msg, State) ->
    lager:warning("unhandled info message ~p", [_Msg]),
    {noreply, State}.

terminate(_Reason, #state{ubx_handle=Handle}) ->
    case is_pid(Handle) of
        true -> ubx:stop(Handle, normal);
        _ -> ok
    end.
