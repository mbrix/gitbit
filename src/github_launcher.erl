-module(github_launcher).

-export([launch/0]).


launch() ->
	{ok, ListenerCount} = application:get_env(gitbit, listeners),
	{ok, Port} = application:get_env(gitbit, port),
	NameId = github_webhooks,
	Dispatch = cowboy_router:compile(
	[
	 {'_', [
	 		{"/webhook", gitbit_github_handler, []},
			{"/v1/client/recent", gitbit_recent_handler, []},
			{"/v1/client/top", gitbit_top_handler, []},
			{"/v1/client/search", gitbit_search_handler, []}
	 	   ]}
	]),
    CowboyOptions = 
    [ {env,       [{dispatch, Dispatch}]}
      , {compress,  true}
      , {timeout,   12000}
    ],
      
    RanchOptions = [ {port, Port}],
    {ok, _} = cowboy:start_http(NameId,
    							ListenerCount,
    							RanchOptions,
    							CowboyOptions),
	lager:info("Launching github webhook listeners").


