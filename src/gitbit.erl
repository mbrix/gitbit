-module(gitbit).

-behaviour(application).

%% Application callbacks
-export([start/0,
		 start/2,
         stop/1]).

%% Launch fun
start() -> application:ensure_all_started(gitbit).

start(_StartType, _StartArgs) ->
	github_launcher:launch(), %%cowboy
    gitbit_sup:start_link().

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
