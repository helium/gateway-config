-module(gateway_ubx).

-behavior(ebus_object).

-define(OBJECT_PATH, "/com/helium/GPS").
-define(OBJECT_INTERFACE, "com.helium.GPS").

%% ebus_object
-export([start_link/2, init/1, handle_info/2, handle_message/3, terminate/2]).

-record(state, {
                ubx_handle :: pid() | undefined,
                gps_lock=false :: boolean(),
                gps_position=#{} :: #{string() => float()}
               }).

start_link(Bus, Args) ->
    ebus_object:start_link(Bus, ?OBJECT_PATH, ?MODULE, Args, []).

init(Args) ->
    Filename = proplists:get_value(filename, Args, ""),
    case (filelib:is_regular("/dev/"++Filename)) of
        true ->
            Gpio = proplists:get_value(gpio, Args, 68),
            {ok, Pid} = ubx:start_link(Filename, Gpio, [], self()),
            ubx:enable_message(nav_posllh, 5, Pid),
            ubx:enable_message(nav_sol, 5, Pid),
            {ok, #state{ubx_handle=Pid}};
        _ ->
            lager:warning("No UBX filename or device found, running in stub mode"),
            {ok, #state{ubx_handle=undefined}}
    end.


handle_message("Position", _Msg, State=#state{}) ->
    {reply,
     [bool, {dict, string, double}],
     [State#state.gps_lock, State#state.gps_position], State};
handle_message(Member, _Msg, State) ->
    lager:warning("Unhandled message ~p", Member),
    {noreply, State}.

handle_info({nav_sol, GPSFix}, State) ->
    case GPSFix == 3 of
        true ->
            {noreply, State#state{gps_lock=true}};
        false ->
            {noreply, State#state{gps_lock=false}}
    end;
handle_info({nav_posllh, _}, State=#state{gps_lock=false}) ->
    {noreply, State};
handle_info({nav_posllh, {Lat,Lon,Height,HorizontalAcc,VerticalAcc}}, State=#state{}) ->
    Position = #{
                 "lat" => Lat,
                 "lon" => Lon,
                 "height" => Height,
                 "h_accuracy" => HorizontalAcc,
                 "v_accuracy" => VerticalAcc
                },
    {noreply, State#state{gps_position=Position},
     {signal, ?OBJECT_PATH, ?OBJECT_INTERFACE, "Position",
      [{dict, string, double}], [Position]}};

handle_info(_Msg, State) ->
    lager:warning("unhandled info message ~p", [_Msg]),
    {noreply, State}.

terminate(_Reason, #state{ubx_handle=Handle}) ->
    case is_pid(Handle) of
        true -> ubx:stop(Handle, normal);
        _ -> ok
    end.