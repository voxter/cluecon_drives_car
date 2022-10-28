-module(cdc_poll_watcher).

-export(
	[ forward/0
	, reverse/0
	, left/0
	, right/0
	]).
-export(
	[ start_link/0
	, start_link/1
	, subscribe/1
	, sentiments/1
	, smirk/0
	]).
-export(
	[ init/1
	, handle_call/3
	, handle_cast/2
	, handle_info/2
	, terminate/2
	, code_change/3
	]).

forward() ->
	gen_server:call(?MODULE, {vote, forward}, infinity).

reverse() ->
	gen_server:call(?MODULE, {vote, reverse}, infinity).

left() ->
	gen_server:call(?MODULE, {vote, left}, infinity).

right() ->
	gen_server:call(?MODULE, {vote, right}, infinity).

sentiments(Feels) ->
	gen_server:call(?MODULE, {sentiments, Feels}, infinity).

smirk() ->
	gen_server:cast(?MODULE, smirk).

start_link() ->
	start_link(#{}).

start_link(Opts) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

subscribe(Callback) ->
	Self = self(),
	gen_server:cast(?MODULE, {subscribe, Self, Callback}).

-record(state,
	{ vote_record = cdc_vote_record:new()
	, pulse_timer
	, pulse_time = 1000
	, subscribers = #{}
	}).

init(Opts)  ->
	PulseTime = maps:get(pulse_time, Opts, 1000),
	State = #state{ pulse_time = PulseTime },
	StateWithPulse = set_pulse_timer(State),
	{ok, StateWithPulse}.

handle_call({vote, VoteName}, _Req, State) ->
	Votes = State#state.vote_record,
	NewVotes = cdc_vote_record:VoteName(Votes),
	WithVotes = State#state{ vote_record = NewVotes},
	CleanedSubs = tell_subs(VoteName, NewVotes, WithVotes),
	{reply, ok, CleanedSubs};

handle_call({sentiments, []}, _From, State) ->
	{reply, ok, State};

handle_call({sentiments, Words}, _From, State) ->
	CleanedSubs = tell_subs(sentimetns, Words, State),
	{reply, ok, CleanedSubs};

handle_call(Req, _From, State) ->
	io:format("Nope, not gunna do ~p~n", [Req]),
	{reply, {error, {nyi, Req}}, State}.

handle_cast({subscribe, Pid, Callback}, State) ->
	OldSubs = State#state.subscribers,
	Mon = erlang:monitor(process, Pid),
	NewSubs = OldSubs#{ {Pid, Mon} => Callback },
	NewState = State#state{ subscribers = NewSubs },
	{noreply, NewState};

handle_cast({sentiments, Feels}, State) ->
	io:format("The feels: ~p~n", [Feels]),
	CleanedSubs = tell_subs(sentiments, Feels, State),
	{noreply, CleanedSubs};

handle_cast(smirk, State) ->
	io:format("smirk?~n"),
	CleanedSubs = tell_subs(smirk, smirk, State),
	{noreply, CleanedSubs};

handle_cast(_, State) ->
	{noreply, State}.

handle_info(pulse, State) ->
	PulseState = set_pulse_timer(State),
	ResetVotes = cdc_vote_record:new(),
	WithResetVotes = PulseState#state{ vote_record = ResetVotes },
	WithNewSubs = tell_subs(pulse, State#state.vote_record, WithResetVotes),
	{noreply, WithNewSubs};

handle_info({'DOWN', Ref, process, Pid, _}, State) ->
	OldSubs = State#state.subscribers,
	Key = {Pid, Ref},
	NewSubs = maps:remove(Key, OldSubs),
	{noreply, State#state{ subscribers = NewSubs}};

handle_info(_, State) ->
	{noreply, State}.

terminate(_, _) -> ok.

code_change(_, State, _) ->
	{ok, State}.

tell_subs(EventName, VoteData, State) ->
	KeepsSubs = maps:filter(fun(_Key, Callback) ->
		tell_sub(Callback, EventName, VoteData)
	end, State#state.subscribers),
	State#state{ subscribers = KeepsSubs}.

tell_sub(Callback, EventName, VoteData) ->
	try Callback(EventName, VoteData) of
		_ ->
			true
	catch
		What:Why ->
			io:format("callback ~p failed: ~p:~p~n", [Callback, What, Why]),
			false
	end.

set_pulse_timer(#state{ pulse_timer = undefined } = State) ->
	PulseTime = State#state.pulse_time,
	Timer = erlang:send_after(PulseTime, self(), pulse),
	State#state{ pulse_timer = Timer };
set_pulse_timer(State) ->
	case erlang:cancel_timer(State#state.pulse_timer) of
		ok ->
			set_pulse_timer(State#state{ pulse_timer = undefined });
		false ->
			ok = consume_pulses(),
			set_pulse_timer(State#state{ pulse_timer = undefined });
		0 ->
			ok = consume_pulses(),
			set_pulse_timer(State#state{ pulse_timer = undefined });
		_N ->
			set_pulse_timer(State#state{ pulse_timer = undefined})
	end.

consume_pulses() ->
	receive
		pulse ->
			consume_pulses()
	after
		0 ->
			ok
	end.
