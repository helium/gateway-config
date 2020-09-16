-module(gateway_config_worker).

-behavior(ebus_object).

-define(WORKER, gateway_config).
-define(APPLICATION, gateway_config).

-include("gateway_config.hrl").
-include_lib("gatt/include/gatt.hrl").
-include_lib("ebus/include/ebus.hrl").

%% ebus_object
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, handle_message/3, terminate/2]).
%% api
-export([advertising_enable/1, advertising_info/0,
         ble_device_info/0]).

-define(ADVERTISING_TIMEOUT, 5 * 60 * 1000).


-record(state, {
                button_handle :: pid() | undefined,
                bluetooth_advertisement=undefined :: pid() | undefined,
                bluetooth_timer=make_ref() :: reference(),
                bluetooth_proxy :: ebus:proxy(),
                p2p_timer=make_ref() :: reference()
               }).

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
    {ok, ButtonArgs} = application:get_env(button),
    ButtonPid = init_button(ButtonArgs),

    {ok, Bus} = ebus:system(),
    {ok, BluezProxy} = ebus_proxy:start_link(Bus, ?BLUEZ_SERVICE, []),

    {ok, #state{bluetooth_proxy=BluezProxy,
                button_handle=ButtonPid}}.


init_button(Args) ->
    case file:read_file_info("/dev/gpio") of
        {ok, _} ->
            Gpio = proplists:get_value(gpio, Args, 7),
            {ok, Pid} = gpio_button:start_link(Gpio, self()),
            Pid;
        _ ->
            lager:warning("No GPIO device tree found, running in stub mode"),
            undefined
    end.


handle_message(Member, _Msg, State) ->
    lager:warning("Unhandled config message ~p", [Member]),
    {reply_error, ?DBUS_ERROR_NOT_SUPPORTED, Member, State}.


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


handle_cast(Msg, State=#state{}) ->
    lager:warning("Unhandled cast ~p", [Msg]),
    {noreply, State}.


%% Button click
handle_info({button_clicked, _, 1}, State=#state{}) ->
    lager:info("Button clicked"),
    %% Start a scan for visible wifi services
    connman:enable(wifi, true),
    connman:scan(wifi),
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

terminate(_Reason, State=#state{}) ->
    case State#state.bluetooth_advertisement of
        undefined -> ok;
        AdvPid -> (catch ble_advertisement:stop(AdvPid, normal))
    end.
