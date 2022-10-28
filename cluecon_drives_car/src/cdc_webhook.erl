-module(cdc_webhook).

-include_lib("inets/include/httpd.hrl").

-export(
	[ init/2 ]
	).

base_headers() ->
	#{
		<<"content-type">> => <<"text/xml">>
	}.

gather(Kids) ->
	HookUrlBase = application:get_env(cluecon_drives_car, webhook_url, ""),
	GatherAttrs = 
		[ {action, HookUrlBase ++ "/gathered?skip_prompt=true"}
		, {actionOnEmptyResult, "true"}
		, {partialResultCallback, HookUrlBase ++ "/gathered"}
		, {partialResultCallbackMethod, "POST"}
		, {hints, "go stop forward reverse left right"}
		, {input, "dtmf speech"}
		, {method, "POST"}
		, {timeout, "1 hour"}
		],
	{'Gather', GatherAttrs, Kids }.

response(Kids) ->
	{'Response', [], Kids}.

init(Req, [call_changes] = State) ->
	{ok, Req, State};
init(Req, [calls] = State) ->
	Headers = base_headers(),

	QsVals = cowboy_req:parse_qs(Req),
	GatherKids = case lists:keyfind(<<"skip_prompt">>, 1, QsVals) of
		false ->
			Prompt = {'Say', [],
				[ "Commands I understand are: "
				  "go, stop, forward, reverse, left, right. "
				  "When done, hit pound. "
				  "You can also press keys. "
				  "To go forward, Press 2. "
				  "To stop or backup, Press 8. "
				  "To turn left, Press 4. "
				  "To turn right, Press 6. "
				  "To repeat this help, hangup and callback." ]},
			[ Prompt ];
		_ ->
			[]
	end,

	Gather = gather( GatherKids ),
	BodyErl = response( [ Gather ] ),
	io:format("Simple: ~p~n", [BodyErl]),
	Body = xmerl:export_simple([BodyErl], xmerl_xml),
	io:format("Encoded: ~p~n", [Body]),
	Req1 = cowboy_req:reply(200, Headers, Body, Req),
	{ok, Req1, State};

init(Req, [message] = State) ->
	{ok, Body, Req1} = cowboy_req:read_body(Req),
	io:format("message webhook body: ~s~n", [Body]),
	ParsedBody = cow_qs:parse_qs(Body),
	Sentiments = do_handle_speech(proplists:get_value(<<"Body">>, ParsedBody, <<>>)),
	_ = cdc_poll_watcher:sentiments(Sentiments),
	Req2 = cowboy_req:reply(204, base_headers(), "", Req1),
	{ok, Req2, State};

init(Req, [gather] = State) ->
	{ok, Body, Req1} = cowboy_req:read_body(Req),
	io:format("The body I got for gather:~n~s~n", [Body]),
	ok = handle_gathered(Body),
	BodyErl = response( [ gather( [] )]),
	OutBody = xmerl:export_simple([BodyErl], xmerl_xml),
	Req2 = cowboy_req:reply(200, base_headers(), OutBody, Req1),
	{ok, Req2, State}.

handle_gathered(Body) ->
	do_handle_gathered(Body),
	ok.

do_handle_gathered(Body) ->
	Parsed = cow_qs:parse_qs(Body),
	DigitString = proplists:get_value(<<"Digits">>, Parsed, <<>>),
	SpeechString = proplists:get_value(<<"SpeechResult">>, Parsed, <<>>),
	do_handle_digits(DigitString),
	Sentiments = do_handle_speech(SpeechString),
	cdc_poll_watcher:sentiments(Sentiments).

do_handle_digits(<<>>) ->
	ok;
do_handle_digits(<<$2, Tail/binary>>) ->
	cdc_poll_watcher:forward(),
	do_handle_digits(Tail);
do_handle_digits(<<$4, Tail/binary>>) ->
	cdc_poll_watcher:left(),
	do_handle_digits(Tail);
do_handle_digits(<<$6, Tail/binary>>) ->
	cdc_poll_watcher:right(),
	do_handle_digits(Tail);
do_handle_digits(<<$8, Tail/binary>>) ->
	cdc_poll_watcher:reverse(),
	do_handle_digits(Tail);
do_handle_digits(<<_/utf8, Tail/binary>>) ->
	do_handle_digits(Tail).

do_handle_speech(String) when is_binary(String) ->
	case string:casefold(String) of
		<<"smirk">> ->
			cdc_car_driver:smirk(),
			cdc_poll_watcher:smirk(),
			[];
		Lcase ->
			Words = string:lexemes(Lcase, " "),
			io:format("Split: ~p~n", [Words]),
			do_handle_speech(Words, [])
	end.

do_handle_speech([], Sentiments) ->
	ordsets:from_list(Sentiments);
do_handle_speech([<<"go">> | Tail], Acc) ->
	cdc_poll_watcher:forward(),
	do_handle_speech(Tail, Acc);
do_handle_speech([<<"forward">> | Tail], Acc) ->
	cdc_poll_watcher:forward(),
	do_handle_speech(Tail, Acc);
do_handle_speech([<<"stop">> | Tail], Acc) ->
	cdc_poll_watcher:reverse(),
	do_handle_speech(Tail, [<<"stop">> | Acc]);
do_handle_speech([<<"reverse">> | Tail], Acc) ->
	cdc_poll_watcher:reverse(),
	do_handle_speech(Tail, Acc);
do_handle_speech([<<"left">> | Tail], Acc) ->
	cdc_poll_watcher:left(),
	do_handle_speech(Tail, Acc);
do_handle_speech([<<"right">> | Tail], Acc) ->
	cdc_poll_watcher:right(),
	do_handle_speech(Tail, Acc);
do_handle_speech([Whatever | Tail], Acc) ->
	NewAcc = maybe_add_sentiment(Whatever, Acc),
	do_handle_speech(Tail, NewAcc).

maybe_add_sentiment(Word, Acc) ->
	Moods = [ {frustation, frustations()}
	, {panic, panic()}
	, {victory, victory()}
	],
	Sentiment = lists:foldl(
		fun
			({Mood, Set}, undefined) ->
				case sets:is_element(Word, Set) of
					false ->
						undefined;
					true ->
						{ok, Mood}
				end;
			(_, MoodAcc) ->
				MoodAcc
		end,
	undefined, Moods),
	case Sentiment of
		undefined ->
			Acc;
		{ok, Mood} ->
			[Mood | Acc]
	end.

frustations() ->
	sets:from_list(
		[ <<"shit">>
		, <<"crap">>
		, <<"fuck">>
		, <<"hell">>
		]).

panic() ->
	sets:from_list(
		[ <<"no">>
		, <<"god">>
		, <<"panic">>
		, <<"crash">>
		]).

victory() ->
	sets:from_list(
		[ <<"yes">>
		, <<"woot">>
		, <<"yeah">>
		, <<"win">>
		]).


