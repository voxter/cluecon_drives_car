-module(sw_api).

-export(
	[ auth_header/0
	, auth_header/2
	, do_the_ws/0
	, raw/3
	, project_id/0
	, token/0
	, host/0
	, full_path/1
	, phone_number/0
	, phone_number_id/0
	, update_webhooks/1
	, project_id/1
	, token/1
	, host/1
	, phone_number/1
	, phone_number_id/1
	]).

project_id() ->
	application:get_env(cluecon_drives_car, project_id, <<>>).

token() ->
	application:get_env(cluecon_drives_car, token, <<>>).

host() ->
	application:get_env(cluecon_drives_car, space_url, "").

full_path(Tail) ->
	filename:join(["/api/relay/rest" | Tail]).

phone_number() ->
	application:get_env(cluecon_drives_car, phone_number, <<>>).

phone_number_id() -> 
	application:get_env(cluecon_drives_car, phone_number_id, <<>>).

project_id(V) ->
	application:set_env(cluecon_drives_car, project_id, V).

token(V) ->
	application:set_env(cluecon_drives_car, token, V).

host(V) ->
	application:set_env(cluecon_drives_car, space_url, V).

phone_number(V) ->
	application:set_env(cluecon_drives_car, phone_number, V).

phone_number_id(V) -> 
	application:set_env(cluecon_drives_car, phone_number_id, V).


update_webhooks(NewUrl) ->
	application:set_env(cluecon_drives_car, webhook_url, NewUrl),
	{ok, Connection} = gun:open(host(), 443),
	{ok, _Proto} = gun:await_up(Connection),
	FullPath = full_path(["phone_numbers", phone_number_id()]),
	BodyErl =
		#{ name => <<"cluecon-drives-car">>
		,  call_handler => laml_webhooks
		,  call_recieve_mode => voice
		,  call_request_url => unicode:characters_to_binary([NewUrl, "/calls"])
		,  call_fallback_url => ""
		,  call_requst_method => 'POST'
		,  call_status_callback_url => unicode:characters_to_binary([NewUrl, "/calls/change"])
		,  call_status_callback_method => 'POST'
		,  message_handler => laml_webhooks
		,  message_request_url => unicode:characters_to_binary([NewUrl, "/messages"])
		,  message_request_method => 'POST'
		},
	Body = jsx:encode(BodyErl),
	Stream = gun:put(Connection, FullPath, [auth_header(), {<<"content-type">>, <<"application/json">>}], Body),
	gun:await_body(Connection, Stream).

auth_header() ->
	auth_header(project_id(), token()).

auth_header(ProjectId, AuthToken) ->
	Combine = unicode:characters_to_binary([ProjectId, $:, AuthToken]),
	Auth = base64:encode(Combine),
	{<<"authorization">>, [<<"Basic ">>, Auth]}.

raw(Method, Path, MaybeBody) ->
	{ok, Connection} = gun:open(host(), 443),
	{ok, _Proto} = gun:await_up(Connection),
	FullPath = full_path(Path),
	Stream = case Method of
		get ->
			gun:get(Connection, FullPath, [auth_header()]);
		put ->
			EncodedBody = encoded_body(MaybeBody),
			gun:put(Connection, FullPath, [auth_header()], EncodedBody);
		post ->
			EncodedBody = encoded_body(MaybeBody),
			gun:post(Connection, FullPath, [auth_header()], EncodedBody);
		patch ->
			EncodedBody = encoded_body(MaybeBody),
			gun:patch(Connection, FullPath, [auth_header()], EncodedBody);
		head ->
			gun:head(FullPath, [auth_header()]);
		options ->
			gun:options(FullPath, [auth_header()])
	end,
	gun:await_body(Connection, Stream).

encoded_body(undefined) ->
	[];
encoded_body(Bin) when is_binary(Bin) ->
	Bin;
encoded_body(Map) when is_map(Map) ->
	jsx:encode(Map).


do_the_ws() ->
	application:ensure_all_started(gun),
	{ok, C} = gun:open("relay.signalwire.com", 443),
	{ok, P} = gun:await_up(C),
	io:format("Das proto: ~p~n", [P]),
	S = gun:ws_upgrade(C, "/"),
	io:format("Das stream: ~p~n", [S]),
	receive
		{gun_upgrade, C, S, Something, Headers} ->
			io:format("Upgrade complete:~n"
				"    C: ~p~n"
				"    S: ~p~n"
				"    Something: ~p~n"
				"    Headers: ~p~n"
				, [C, S, Something, Headers]),
			ok;
		Wut ->
			io:format("no idea: ~p~n", [Wut]),
			error(Wut)
	end,
	ConnectReq = jsx:encode(
		#{ jsonrpc => <<"2.0">>
		,  id => <<"some-id">>
		,  method => <<"signalwire.connect">>
		,  params =>
			#{ agent => <<"erlang-gun">>
			,  version =>
				#{ major => 3
				,  minor => 0
				,  revision => 0
				}
			, authentication =>
				#{ project => project_id()
				,  token => token()
				}
			}
		}
	),
	ok = gun:ws_send(C, S, ConnectReq),
	Wait = 5000,
	eat_it_all(Wait).

eat_it_all(Wait) ->
	receive
		M ->
			io:format("~p~n", [M]),
			eat_it_all(Wait)
	after
		Wait ->
			ok
	end.