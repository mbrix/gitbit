-module(gitbit_sup).

-behaviour(supervisor).

%% API functions
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    {ok, {{one_for_one, 5, 10},
    	  [?CHILD(gitbit_manager, gitbit_manager, worker, []),
    	   ?CHILD(ebitcoind_rpc, ebitcoind, worker, [])]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
