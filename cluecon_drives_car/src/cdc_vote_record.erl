-module(cdc_vote_record).

-export(
	[ new/0
	, forward/1
	, reverse/1
	, left/1
	, right/1
	, wheel_actions/1
	, to_map/1
	]).

-record(vote_record,
	{ right_forward = 0
	, right_backward = 0
	, left_forward = 0
	, left_backward =  0
	}).

-type vote_record() :: #vote_record{}.

-export_type([vote_record/0]).

forward(Rec) ->
	increment(Rec, [#vote_record.right_forward, #vote_record.left_forward]).

reverse(Rec) ->
	increment(Rec, [#vote_record.right_backward, #vote_record.left_backward]).

left(Rec) ->
	increment(Rec, [#vote_record.right_forward, #vote_record.left_backward]).

right(Rec) ->
	increment(Rec, [#vote_record.right_backward, #vote_record.left_forward]).

new() ->
	#vote_record{}.

to_map(Rec) ->
	#{ right_forward => Rec#vote_record.right_forward
	,  right_backward => Rec#vote_record.right_backward
	,  left_forward => Rec#vote_record.left_forward
	,  left_backward => Rec#vote_record.left_backward
	}.

wheel_actions(Rec) ->
	RightForward = Rec#vote_record.right_forward,
	RightBackward = Rec#vote_record.right_backward,
	LeftForward = Rec#vote_record.left_forward,
	LeftBackward = Rec#vote_record.left_backward,
	RightRatio = ratio(RightForward, RightBackward),
	LeftRatio = ratio(LeftForward, LeftBackward),
	#{ right => RightRatio, left => LeftRatio }.

increment(Tuple, FieldList) ->
	lists:foldl(fun(Idx, Acc) ->
		Old = element(Idx, Acc),
		setelement(Idx, Acc, Old + 1)
	end, Tuple, FieldList).

ratio(Forward, Reverse) ->
	Total = Forward + Reverse,
	if
		Total == 0 ->
			0;
		true ->
			Diff = Forward - Reverse,
			Diff / Total
	end.