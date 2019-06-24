-module(gateway_config_worker).

-behavior(ebus_object).

-define(WORKER, gateway_config).
-define(APPLICATION, gateway_config).

-include("gateway_config.hrl").
-include("gateway_gatt.hrl").
-include_lib("gatt/include/gatt.hrl").
-include_lib("ebus/include/ebus.hrl").

%% ebus_object
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, handle_message/3, terminate/2]).
%% api
-export([gps_info/0, gps_sat_info/0, gps_offline_assistance/1, gps_online_assistance/1,
         download_info/0, download_info/1,
         advertising_enable/1, advertising_info/0,
         ble_device_info/0]).

-define(ADVERTISING_TIMEOUT, 5 * 60 * 1000).


-record(state, {
                ubx_handle :: pid() | undefined,
                button_handle :: pid() | undefined,
                bluetooth_advertisement=undefined :: pid() | undefined,
                bluetooth_timer=make_ref() :: reference(),
                bluetooth_proxy :: ebus:proxy(),
                gps_lock=false :: boolean(),
                gps_info=#{} :: ubx:nav_pvt()| #{},
                gps_sat_info=[] :: [ubx:nav_sat()],
                download_info=false :: boolean()
               }).

gps_info() ->
    gen_server:call(?WORKER, gps_info).

gps_sat_info() ->
    gen_server:call(?WORKER, gps_sat_info).

gps_offline_assistance(Path) ->
    gen_server:call(?WORKER, {gps_offline_assistance, Path}).

gps_online_assistance(Path) ->
    gen_server:call(?WORKER, {gps_online_assistance, Path}).

download_info() ->
    gen_server:call(?WORKER, download_info).

download_info(Value) ->
    gen_server:cast(?WORKER, {download_info, Value}).

advertising_enable(Enable) ->
    ?WORKER ! {enable_advertising, Enable}.

advertising_info() ->
    gen_server:call(?WORKER, advertising_info).

ble_device_info() ->
    gen_server:call(?WORKER, ble_device_info).

%% ebus_object

start_link(Bus) ->
    ok = ebus:request_name(Bus, ?CONFIG_APPLICATION_NAME),
    ebus_object:start_link(Bus, ?CONFIG_OBJECT_PATH, ?MODULE, [], []).


init(_) ->
    erlang:register(?WORKER, self()),
    {ok,GpsArgs} = application:get_env(gps),
    UbxPid = init_ubx(GpsArgs),
    {ok, ButtonArgs} = application:get_env(button),
    ButtonPid = init_button(ButtonArgs),

    {ok, Bus} = ebus:system(),
    {ok, Proxy} = ebus_proxy:start_link(Bus, ?BLUEZ_SERVICE, []),

    {ok, #state{ubx_handle=UbxPid,
                bluetooth_proxy=Proxy,
                button_handle=ButtonPid}}.


init_ubx(Args) ->
    Filename = proplists:get_value(filename, Args),
    case (file:read_file_info("/dev/"++Filename)) of
        {ok, _} ->
            Gpio = proplists:get_value(gpio, Args, 68),
            {ok, Pid} = ubx:start_link(Filename, Gpio, [], self()),
            ubx:disable_message(Pid, nav_sol),
            ubx:disable_message(Pid, nav_posllh),
            ubx:enable_message(Pid, nav_pvt, 5),
            ubx:enable_message(Pid, nav_sat, 5),
            sync_clocks(Pid),
            Pid;
        _ ->
            lager:warning("No UBX filename or device found, running in stub mode"),
            undefined
    end.


init_button(Args) ->
    case file:read_file_info("/dev/gpio") of
        {ok, _} ->
            Gpio = proplists:get_value(gpio, Args, 90),
            {ok, Pid} = gpio_button:start_link(Gpio, self()),
            Pid;
        _ ->
            lager:warning("No GPIO device tree found, running in stub mode"),
            undefined
    end.

sync_clocks(UbxPid) ->
    case calendar:universal_time() of
        {{1970, _, _}, _} ->
            set_system_time(UbxPid);
        DateTime ->
            ubx:set_time_utc(UbxPid, DateTime)
    end.

set_system_time(UbxPid) ->
    case ubx:poll_message(UbxPid, nav_timeutc) of
        {ok, {nav_timeutc, #{datetime := {{Year, Month, Day}, {Hour, Min, Sec}}}}} ->
            Ymd = io_lib:format("~b-~2..0b-~2..0b", [Year, Month, Day]),
            Hms = io_lib:format("~2..0b:~2..0b:~2..0b", [Hour, Min, Sec]),
            lager:info("Setting date to ~p ~p", [Ymd, Hms]),
            os:cmd("date -s '" ++ Ymd ++ " " ++ Hms ++ "'");
        _ ->
            lager:warning("No valid UTC datetime found")
    end.


handle_message(?CONFIG_OBJECT(?CONFIG_MEMBER_POSITION), _Msg, State=#state{}) ->
    Position = navmap_to_position(State#state.gps_info),
    {reply,
     [bool, {dict, string, variant}],
     [State#state.gps_lock, Position], State};
handle_message(?CONFIG_OBJECT(?CONFIG_MEMBER_DOWNLOADING), _Msg, State=#state{}) ->
    {reply, [bool], [State#state.download_info], State};

handle_message(Member, _Msg, State) ->
    lager:warning("Unhandled config message ~p", [Member]),
    {reply_error, ?DBUS_ERROR_NOT_SUPPORTED, Member, State}.


handle_call(gps_info, _From, State=#state{}) ->
    {reply, State#state.gps_info, State};
handle_call(gps_sat_info, _From, State=#state{}) ->
    {reply, State#state.gps_sat_info, State};
handle_call({gps_offline_assistance, Path}, _From, State=#state{ubx_handle=Handle}) ->
    {reply, ubx:upload_offline_assistance(Handle, Path), State};
handle_call({gps_online_assistance, Path}, _From, State=#state{ubx_handle=Handle}) ->
    {reply, ubx:upload_online_assistance(Handle, Path), State};
handle_call(download_info, _From, State=#state{}) ->
    {reply, State#state.download_info, State};
handle_call(advertising_info, _From, State=#state{}) ->
    Adv = case State#state.bluetooth_advertisement of
              undefined -> off;
              _ -> on
          end,
    {reply, Adv, State};
handle_call(ble_device_info, _From, State=#state{}) ->
    case ebus_proxy:call(State#state.bluetooth_proxy, ?DBUS_OBJECT_MANAGER("GetManagedObjects")) of
        {ok, [Map]} ->
            {reply, {ok, lists:filtermap(fun(#{"org.bluez.Device1" :=
                                                   D=#{"Adapter" := ?CONFIG_BLE_ADAPTER_PATH}}) ->
                                                 {true, D};
                                            (_) -> false end,
                                         maps:values(Map))}, State};
        {error, Error} ->
            {reply, {error, Error}, State}
    end;


handle_call(Msg, _From, State=#state{}) ->
    lager:warning("Unhandled call ~p", [Msg]),
    {reply, ok, State}.


handle_cast({download_info, Value}, State=#state{}) ->
    {noreply, State#state{download_info=Value},
     {signal, ?CONFIG_OBJECT_PATH, ?CONFIG_OBJECT_INTERFACE, ?CONFIG_MEMBER_DOWNLOADING,
      [bool], [Value]}};

handle_cast(Msg, State=#state{}) ->
    lager:warning("Unhandled cast ~p", [Msg]),
    {noreply, State}.


handle_info({nav_pvt, NavMap}, State=#state{}) ->
    report_gps_stats(NavMap),
    FixType = maps:get(fix_type, NavMap, 0),
    case update_gps_lock(FixType, State) of
        {true, NewState} ->
            lager:debug("Signaling GPS lock ~p", [NewState#state.gps_lock]),
            maybe_signal_position(NewState),
            {noreply, NewState#state{gps_info=NavMap},
             {signal, ?CONFIG_OBJECT_PATH, ?CONFIG_OBJECT_INTERFACE, ?CONFIG_MEMBER_POSITION_LOCK,
              [bool], [NewState#state.gps_lock]}};
        {false, NewState} ->
            maybe_signal_position(NewState),
            {noreply, NewState#state{gps_info=NavMap}}
    end;
handle_info({nav_sat, NavSats}, State=#state{}) ->
    {noreply, State#state{gps_sat_info=NavSats}};
handle_info(signal_position, State=#state{}) ->
    Position = navmap_to_position(State#state.gps_info),
    lager:debug("Signaling locked GPS position ~p", [Position]),
    {noreply, State,
     {signal, ?CONFIG_OBJECT_PATH, ?CONFIG_OBJECT_INTERFACE, ?CONFIG_MEMBER_POSITION,
      [{dict, string, variant}], [Position]}};
handle_info({packet, Packet}, State=#state{}) ->
    lager:notice("Ignoring unrequested ubx packet: ~p", [Packet]),
    {noreply, State};

%% Button click
handle_info({button_clicked, _, 1}, State=#state{}) ->
    lager:info("Button clicked"),
    handle_info({enable_advertising, true}, State);

%% BLE Advertising
handle_info({enable_advertising, true}, State=#state{bluetooth_advertisement=undefined}) ->
    lager:info("Enabling advertising"),
    {ok, Bus} =  gateway_gatt_application:bus(),
    {ok, AdvPid} = ble_advertisement:start_link(Bus, gateway_gatt_application:path(), 0,
                                                gateway_ble_advertisement, []),
    erlang:cancel_timer(State#state.bluetooth_timer),
    Timer = erlang:send_after(?ADVERTISING_TIMEOUT, self(), timeout_advertising),
    {noreply, State#state{bluetooth_advertisement=AdvPid, bluetooth_timer=Timer}};
handle_info({enable_advertising, false}, State=#state{bluetooth_advertisement=Pid}) when is_pid(Pid) ->
    lager:info("Disable advertising"),
    ble_advertisement:stop(Pid, normal),
    erlang:cancel_timer(State#state.bluetooth_timer),
    {noreply, State#state{bluetooth_advertisement=undefined}};
handle_info({enable_advertising, _}, State=#state{}) ->
    lager:debug("Unchanged advertising state"),
    {noreply, State};
handle_info(timeout_advertising, State=#state{})  ->
    lager:info("Timeout advertising"),
    handle_info({enable_advertising, false}, State);


handle_info(_Msg, State) ->
    lager:warning("unhandled info message ~p", [_Msg]),
    {noreply, State}.

terminate(_Reason, State=#state{ubx_handle=UBXHandle}) ->
    case State#state.bluetooth_advertisement of
        undefined -> ok;
        AdvPid -> (catch ble_advertisement:stop(AdvPid, normal))
    end,
    case is_pid(UBXHandle) of
        true -> ubx:stop(UBXHandle, normal);
        _ -> ok
    end.

%%
%% Internal
%%

update_gps_lock(3, State=#state{gps_lock=false}) ->
    %% If we get a lock signal lock
    {true, State#state{gps_lock=true}};
update_gps_lock(_, State=#state{gps_lock=false}) ->
    %% No lock and fix, don't signal lock
    {false, State};
update_gps_lock(3, State=#state{gps_lock=true}) ->
    %% We maintain a lock, don't signal lock, signal posiiton
    {false, State};
update_gps_lock(_, State=#state{gps_lock=true}) ->
    %% We lost a lock, signal lock
    {true, State#state{gps_lock=false}}.


navmap_to_position(Map) when map_size(Map) == 0 ->
    #{};
navmap_to_position(#{
                     lat := Lat,
                     lon := Lon,
                     height := Height,
                     h_acc := HorizontalAcc,
                     v_acc := VerticalAcc
                    }) ->
    #{
      "lat" => Lat,
      "lon" => Lon,
      "height" => Height,
      "h_accuracy" => HorizontalAcc,
      "v_accuracy" => VerticalAcc
     }.

maybe_signal_position(#state{gps_lock=true}) ->
    self() ! signal_position;
maybe_signal_position(_) ->
    ok.


-spec report_gps_stats(ubx:nav_pvt()) -> ok.
report_gps_stats(NavMap) when map_size(NavMap) == 0 ->
    ok;
report_gps_stats(#{num_sats := NumSats, t_acc := TimeAcc, h_acc := HorizontalAcc, fix_type := FixType}) ->
    dogstatsd:gauge([
                     {"gps.num_sats", NumSats, #{fix_type => FixType}},
                     {"gps.t_acc", TimeAcc, #{fix_type => FixType}},
                     {"gps.h_acc", HorizontalAcc, #{fix_type => FixType}}
                    ]).
