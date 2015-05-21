-module(gitbit_utils).
-include("include/gitbit.hrl").

-export([timestamp/0,
		 to_json/1,
		 options/1]).

timestamp() ->
	{Mega, Sec, Micro} = os:timestamp(),
	Mega * 1000000 *  + Sec * 1000000 + Micro.

to_json(B) ->
	TrimmedDesc = trim(B#bounty.desc),
	#{ name => B#bounty.name,
	   repo => B#bounty.repo,
	   addr => B#bounty.addr,
	   ctime => B#bounty.ctime,
	   endtime => B#bounty.endtime,
	   total => B#bounty.total,
	   status => B#bounty.status,
	   payee => B#bounty.payee,
	   txid => B#bounty.txid,
	   desc => TrimmedDesc,
	   url => B#bounty.url,
	   brag => B#bounty.brag}.

trim(<<N:160/binary, _/binary>>) -> N;
trim(Other) -> Other.

options(Req) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, OPTIONS">>, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req1),
    {ok, Req2}.
