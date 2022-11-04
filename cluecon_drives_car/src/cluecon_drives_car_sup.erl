-module(cluecon_drives_car_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Dispatch = cowboy_router:compile([
		{'_',
			[ {"/calls", cdc_webhook, [calls]}
			, {"/calls/change", cdc_webhook, [call_changes]}
			, {"/messages", cdc_webhook, [message]}
			, {"/gathered", cdc_webhook, [gather]}
			, {"/dashboard", cdc_dashboard_ws, []}
			]}
	]),

	WebServerPort = case os:getenv("WEB_SERVER_PORT") of
		false -> 7000;
		WebServerPortStr -> list_to_integer(WebServerPortStr)
	end,

	{ok, _} = cowboy:start_clear(cdc_webhook, [{port, WebServerPort}], #{env => #{ dispatch => Dispatch }}),

	% this is terrible, but some erlang devs aren't really erlang devs.
	_ = serial:start(),

	Procs = 
		[ #{ id => cdc_poll_watcher
		  ,  start => {cdc_poll_watcher, start_link, []}
		  }
		, #{ id => cdc_car_driver
		  ,  start => {cdc_car_driver, start_link, []}
		  }
		],

	{ok, {{rest_for_one, 1, 5}, Procs}}.
