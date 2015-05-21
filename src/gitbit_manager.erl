-module(gitbit_manager).

-behaviour(gen_server).

%% API functions
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

% External API
-export([new_bounty/4,
		 complete_bounty/4,
		 top50/0,
		 recent/0,
		 search_by_repo/2,
		 all/0]).

-include("include/gitbit.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(SERVER, ?MODULE).
-define(TXFEE, 0.00010000).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
	% Initialize Mnesia
	init_mnesia(),
    {ok, #{}}.

handle_call({new_bounty, Name, Repo, Desc, Url}, _From, State) ->
	{ok, Address} = ebitcoind:getnewaddress(getrpc()),
	Fun = fun() ->
				  case mnesia:read(bounty, {Name, Repo}) of
				  	  [] ->
				  	  	  mnesia:write(bounty,
				  	  	  			   #bounty{id = {Name, Repo},
				  	  	  			   		   name = Name,
				  	  	  			   		   addr = Address,
				  	  	  			   		   repo = Repo,
				  	  	  			   		   desc = Desc,
				  	  	  			   		   url = Url,
				  	  	  			   		   ctime = gitbit_utils:timestamp(),
				  	  	  			   		   status = new}, write);
				  	  [_] -> ok
				  end end,
	ok = mnesia:activity(transaction, Fun),
	{reply, ok, State};

handle_call({complete_bounty, Name, Repo, Payee, Brag}, _From, State) ->
	lager:info("Completing bounty ~p ~p", [Repo, Name]),
	Fun = fun() -> mnesia:read(bounty, {Name, Repo}) end,
	{reply,
	 try_complete_bounty(mnesia:activity(transaction, Fun), Payee, Brag),
	 State}.

handle_cast({bounty_paid, Bounty, TxId}, State) ->
	Fun = fun() ->
				  mnesia:write(bounty, Bounty#bounty{txid = TxId,
				  									 status = paid,
													 endtime = gitbit_utils:timestamp()},
							   write)
		  end,
	ok = mnesia:activity(transaction, Fun),
	{noreply, State};

handle_cast({bounty_failure, Bounty, Error}, State) ->
	%% Error in closing this bounty
	%% Let's log this error for now
	lager:error("Bounty error: ~p ~p", [Bounty, Error]),
	{noreply, State};

handle_cast({bounty_remove, Bounty, Reason}, State) ->
	%% Close bounty
	lager:info("Bounty removed for ~p", [Reason]),
	Fun = fun() ->
				  mnesia:write(bounty, Bounty#bounty{status = removed,
													 endtime = gitbit_utils:timestamp()},
							   write)
		  end,
	ok = mnesia:activity(transaction, Fun),
	{noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
init_mnesia() ->
	lager:info("Initializing mnesia bounty db"),
	mnesia:create_schema(nodes()),
	mnesia:start(),	
   	mnesia:create_table(bounty,
	[{attributes, record_info(fields, bounty)},
		{disc_copies, nodes()},
    	{type, set}]),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    mnesia:change_table_copy_type(bounty, node(), disc_copies),
	mnesia:wait_for_tables([bounty], infinity).

try_complete_bounty([], _, _) -> missing_bounty;
try_complete_bounty([#bounty{status=paid}],   _Payee, _Brag) -> paid;
try_complete_bounty([#bounty{status=queued}], _Payee, _Brag) -> queued;
try_complete_bounty([#bounty{status=new}=B], Payee, Brag) ->
	C = B#bounty{status = queued, brag = Brag, payee = Payee},
	Fun = fun() -> mnesia:write(bounty, C, write) end,
	ok = mnesia:activity(transaction, Fun),
	%% Lets pay the bounty
	pay_bounty(C, Payee),
	{ok, queued}.

pay_bounty(B, Payee) ->
	spawn(fun() ->
				  case send_money(B#bounty.addr, Payee) of
					  {success, Txid} ->
						  gen_server:cast(?SERVER, {bounty_paid, B, Txid});
					  {failure, insufficient_funds} ->
					  	  gen_server:cast(?SERVER, {bounty_remove, B, insufficient_funds});
					  {failure, Error} ->
						  gen_server:cast(?SERVER, {bounty_failure, B, Error})
				  end
		  end).

send_money(FromAddress, ToAddress) ->
	%% Lets build a btc transaction with inputs only from FromAddress
	%% and send them to ToAddress
	Pid = getrpc(),
	{ok, Unspents} = ebitcoind:listunspent(Pid, 0),
	{Inputs, Total} = filter_unspents(Unspents, FromAddress),
	% Lets take a standard fee
	AdjustedTotal = Total - ?TXFEE,
	if AdjustedTotal < 0 ->
		   {failure, insufficient_funds};
	   true ->
	       {ok, RawTx} = ebitcoind:createrawtransaction(Pid,
	       											 Inputs,
	       											 maps:put(ToAddress,
	       											 		  AdjustedTotal,
	       													  #{})),
	       %% Now submit the raw TX
		   {ok, Response} = ebitcoind:signrawtransaction(Pid, RawTx, []),
		   SignedTx = maps:get(<<"hex">>, Response),
		   Complete = maps:get(<<"complete">>, Response),
		   Complete = true,
		   lager:info("Signedtx: ~p", [SignedTx]),
		   {ok, TxHash} = ebitcoind:sendrawtransaction(Pid, SignedTx),
		   lager:info("Txhash: ~p", [TxHash]),
	       {success, TxHash}
	end.


filter_unspents(Unspents, Address) -> filter_unspents(Unspents, Address, 0, []).
filter_unspents([], _Address, Total, Acc) -> {Acc, Total};
filter_unspents([Unspent|T], Address, Total, Acc) ->
	Spendable = maps:get(<<"spendable">>, Unspent),
	UnspentAddress = maps:get(<<"address">>, Unspent),
	case Spendable of
		true ->
			case UnspentAddress of
				Address ->
					Amount = maps:get(<<"amount">>, Unspent),
					Input = #{<<"txid">> => maps:get(<<"txid">>, Unspent),
							  <<"vout">> => maps:get(<<"vout">>, Unspent)},
					filter_unspents(T, Address, Total+Amount, [Input|Acc]);
				_ -> filter_unspents(T, Address, Total, Acc)
			end;
		_ -> filter_unspents(T, Address, Total, Acc)
	end.

getrpc() ->
	[{ebitcoind_rpc, Pid, _, _},_] = supervisor:which_children(gitbit_sup),
	Pid.


%% Supporting functions
ctimefun() ->
	fun(A, B) -> B#bounty.ctime > A#bounty.ctime end.

totalfun() ->
	fun(A, B) -> B#bounty.total > A#bounty.total end.

search_by_status(Status, OrderFun, Total) ->
    Fun = fun() ->
        Query = qlc:q([B || B <- mnesia:table(bounty), B#bounty.total > Total, B#bounty.status == Status]),
                qlc:eval(qlc:sort(Query, {order, OrderFun}))
    end,
    mnesia:activity(transaction, Fun).

search_by_repo(RepoName, OrderFun) ->
    Fun = fun() ->
        Query = qlc:q([B || B <- mnesia:table(bounty), B#bounty.repo == RepoName]),
                qlc:eval(qlc:sort(Query, {order, OrderFun}))
    end,
    mnesia:activity(transaction, Fun).


%%%===================================================================
%%% External functions 
%%%===================================================================

new_bounty(Name, Repo, Desc, Url) ->
	gen_server:call(?SERVER, {new_bounty, Name, Repo, Desc, Url}).

complete_bounty(Name, Repo, Payee, Brag) ->
	gen_server:call(?SERVER, {complete_bounty, Name, Repo, Payee, Brag}).


%% Search functions (not gen_server)
top50() -> search_by_status(new, totalfun(), -1).

recent() ->	search_by_status(new, ctimefun(), -1).

all() ->
	F = fun() -> mnesia:select(bounty,[{'_',[],['$_']}]) end,
	mnesia:activity(transaction, F).
