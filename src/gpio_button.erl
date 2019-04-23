-module(gpio_button).

-behavior(gen_statem).

-record(data, {
               owner :: pid(),
               gpio :: pid(),
               gpio_num :: pos_integer(),
               click_timer :: reference(),
               click_timeout :: pos_integer(),
               click_count=0 :: non_neg_integer()
              }).

-define(DEFAULT_LONG_PRESS_TIMEOUT, 3000).
-define(DEFAULT_CLICK_TIMEOUT, 300).

%% gen_statem
-export([callback_mode/0, start_link/2, start_link/3, init/1,
        idle/3, pressed/3, clicked/3]).


%%
%% State handling
%%

callback_mode() ->
    state_functions.

start_link(GpioNum, Owner) ->
    start_link(GpioNum, Owner, []).

start_link(GpioNum, Owner, Options) ->
    gen_statem:start_link(?MODULE, [GpioNum, Owner, Options], []).

init([GpioNum, Owner, Options]) ->
    {ok, Gpio} = gpio:start_link(GpioNum, input),
    gpio:register_int(Gpio),
    gpio:set_int(Gpio, both),

    ClickTimeout = proplists:get_value(click_timeout, Options, ?DEFAULT_CLICK_TIMEOUT),
    {ok, idle, #data{owner=Owner, gpio=Gpio, gpio_num=GpioNum,
                     click_timer=make_ref(), click_timeout=ClickTimeout
                    }}.

idle(info, {gpio_interrupt, GpioNum, falling}, #data{gpio_num=GpioNum}) ->
    keep_state_and_data;
idle(info, {gpio_interrupt, GpioNum, rising}, Data=#data{gpio_num=GpioNum}) ->
    {next_state, pressed, button_pressed(Data)};

idle(EventType, Msg, Data) ->
    handle_event(EventType, Msg, Data).


pressed(info, {gpio_interrupt, GpioNum, rising}, #data{gpio_num=GpioNum}) ->
    keep_state_and_data;
pressed(info, {gpio_interrupt, GpioNum, falling}, Data=#data{gpio_num=GpioNum}) ->
    {next_state, clicked, button_clicked(Data)};
pressed(info, long_press_timeout, Data=#data{owner=Owner}) ->
    Owner ! {button_long_press, Data#data.gpio_num},
    {next_state, idle, button_idle(Data)};

pressed(EventType, Msg, Data) ->
    handle_event(EventType, Msg, Data).

clicked(info, {gpio_interrupt, GpioNum, rising}, Data=#data{gpio_num=GpioNum}) ->
    {next_state, pressed, button_pressed(Data)};
clicked(info, {gpio_interrupt, GpioNum, falling}, #data{gpio_num=GpioNum}) ->
    keep_state_and_data;
clicked(info, click_timeout, Data=#data{owner=Owner}) ->
    Owner ! {button_clicked, Data#data.gpio_num, Data#data.click_count},
    {next_state, idle, button_idle(Data)};

clicked(EventType, Msg, Data) ->
    handle_event(EventType, Msg, Data).

%%
%% Utilities
%%

handle_event(info, long_press_timeout, #data{}) ->
    %% handled only in state pressed
    keep_state_and_data;
handle_event(info, click_timeout, #data{}) ->
    %% handled only in state clicked
    keep_state_and_data;
handle_event(EventType, Msg, #data{}) ->
    lager:warning("Unhandled event ~p: ~p", [EventType, Msg]),
    keep_state_and_data.

-spec button_pressed(#data{}) -> #data{}.
button_pressed(Data=#data{}) ->
    erlang:cancel_timer(Data#data.click_timer),
    Data#data{}.

-spec button_idle(#data{}) -> #data{}.
button_idle(Data=#data{}) ->
    erlang:cancel_timer(Data#data.click_timer),
    Data#data{click_count=0}.

-spec button_clicked(#data{}) -> #data{}.
button_clicked(Data=#data{}) ->
    erlang:cancel_timer(Data#data.click_timer),
    Timer = erlang:send_after(Data#data.click_timeout, self(), click_timeout),
    Data#data{click_count=Data#data.click_count + 1, click_timer=Timer}.
