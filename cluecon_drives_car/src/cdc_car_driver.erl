-module(cdc_car_driver).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([tell_me/2]).
-export([car_port/2, car_port/0, reset_port/0]).
-export([smirk/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
	serial_port
	, smirk_mode = false
}).

-define(FORWARD, 1).
-define(BACKWARD, 2).

%% API.

tell_me(pulse, Votes) ->
	gen_server:call(?MODULE, {pulse, Votes}, infinity);
tell_me(_, _) ->
	ok.

smirk() ->
	gen_server:cast(?MODULE, smirk).

car_port() ->
	car_port("/dev/cu.HC-06-Port", 115200).

car_port(Port, Baud) ->
	gen_server:call(?MODULE, {car_port, Port, Baud}, infinity).

reset_port() ->
	gen_server:cast(?MODULE, reset_port).

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server.

init([]) ->
	Yarp = cdc_poll_watcher:subscribe(fun ?MODULE:tell_me/2),
	io:format("The yarp: ~p~n", [Yarp]),
	{ok, #state{}}.

handle_call({car_port, Port, Baud}, _From, State) ->
	CleanedOld = case State#state.serial_port of
		undefined ->
			State;
		{_, OldPort} ->
			OldPort ! {close}
	end,
	New = serial:start([{open, Port}, {speed, Baud}]),
	Mon = erlang:monitor(process, New),
	NewState = CleanedOld#state{ serial_port = {Mon, New} },
	{reply, New, NewState};

handle_call({pulse, _Votes}, _From, #state{ serial_port = undefined} = State) ->
	%io:format("no car to send to :(~n", []),
	{reply, {error, nocar}, State};

handle_call({pulse, _Votes}, _From, #state{ smirk_mode = true} = State) ->
	io:format("HAHAHAHAHAHHAHAHAHAHAHHAHAHAHAHAHHAHAHAHAHAHHAHAHAHAHAHHAHAHAHAHAHHAHAHAHAHAHHAHAHAHAHAHHAHAHAHAHAHHAHAHAHAHAHHAHAHAHAHAHHAHAHAHAHAHHAHAHAHAHAHHAHAHAHAHAHHAHAHAHAHAHHAHAHAHAHAHHAHAHAHAHAHHAHAHAHAHAHHAHAHAHAHAHHAHAHAHAHAHHAHAHAHAHAHHAHAHAHAHAHHAHAHAHAHAHAAAAAAAAAAAAAAAAAAAAAAAHAHAHAHAHAHA"),
	{reply, ok, State};

handle_call({pulse, Votes}, _From, State) ->
	WheelActions = cdc_vote_record:wheel_actions(Votes),
	#{ right := RightAction, left := LeftAction } = WheelActions,
	RightSpeed = as_speed(RightAction),
	LeftSpeed = as_speed(LeftAction),
	RightDirection = as_direction(RightAction),
	LeftDirection = as_direction(LeftAction),

	Message = build_send(RightDirection, RightSpeed, LeftDirection, LeftSpeed),

	{_, SerialPid} = State#state.serial_port,
	SerialPid ! {send, Message},

	io:format("The driver should try to move as such: ~p~n", [WheelActions]),
	{reply, ok, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(reset_port, #state{ serial_port = undefined } = State) ->
	{noreply, State};

handle_cast(reset_port, State) ->
	{_, Pid} = State#state.serial_port,
	Pid ! {send, <<16#10, 16#10, 16#10, 16#10, 16#10, 16#10, 16#10, 16#10, 16#10 >>},
	{noreply, State};

handle_cast(smirk, #state{ serial_port = undefined} = State) ->
	{noreply, State};

handle_cast(smirk, State) ->
	WithSmirk = State#state{ smirk_mode = true },
	{_, Pid} = State#state.serial_port,
	Message = build_send(1, 255, 1, 255),
	Pid ! {send, Message},
	erlang:send_after(5000, self(), smirk),
	{noreply, WithSmirk};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({'DOWN', Mon, process, Pid, Why}, #state{ serial_port = {Mon, Pid} } = State) ->
	io:format("car port died due to ~p~n", [Why]),
	{noreply, State#state{ serial_port = undefined}};

handle_info(smirk, State) ->
	{noreply, State#state{ smirk_mode = false }};

handle_info(Info, State) ->
	io:format("Data; ~p~n", [Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

as_speed(Float) ->
	Abs = abs(Float),
	BigFloat = 255 * Abs,
	round(BigFloat).

as_direction(N) when N < 0 ->
	?BACKWARD;
as_direction(_) ->
	?FORWARD.

build_send(RD, RS, LD, LS) ->
	<<16#ff, 16#55, 16#6, 16#2, 16#36, LD, LS, RD, RS>>.
