-module(cdc_dashboard_ws).

-export([tell_me/3]).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

tell_me(Pid, Event, Votes) ->
	Pid ! {poll_event, Event, Votes}.

-record(state, {
}).

init(Req, State) ->
	{cowboy_websocket, Req, State, #{ idle_timeout => 60000 * 60 }}.

websocket_init(_Opts) ->
	io:format("dashboard socket up~n"),
	Self = self(),
	_ = cdc_poll_watcher:subscribe(fun(A, B) ->
		?MODULE:tell_me(Self, A, B)
	end),
	{ok, #state{}}.

websocket_handle(_Frame, State) ->
	{ok, State}.

websocket_info({poll_event, Event, Data}, State) ->
	%VoteAsMap = cdc_vote_record:to_map(Votes),
	%WheelAction = cdc_vote_record:wheel_actions(Votes),
	case Event of
		pulse ->
			{ok, State};
		sentiments ->
			io:format("The feels: ~p~n", [Data]),
			Frames = lists:map(fun(W) ->
				{text, atom_to_binary(W, utf8)}
			end, Data),
			{Frames, State};
		Word ->
			Frame = {text, atom_to_binary(Word, utf8)},
			{[Frame], State}
	end;

websocket_info(_Info, State) ->
	{ok, State}.
