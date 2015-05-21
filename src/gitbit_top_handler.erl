-module(gitbit_top_handler).
-include("include/gitbit.hrl").

-export([init/2]).
-define(BODY_LIMIT, 1024).

init(Req, Opts) ->
	Method = cowboy_req:method(Req),
	Req2 = validate_req(Method, Req),
	{ok, Req2, Opts}.

validate_req(<<"GET">>, Req) ->
	recent(Req);
validate_req(_, Req) ->
	cowboy_req:reply(405, Req).

send_response(Req, Response) ->
	{ok, Req2} = gitbit_utils:options(Req),
	cowboy_req:reply(200, [
						   {<<"content-type">>, <<"application/json">>}
						  ], Response, Req2).

recent(Req) ->
	Res = gitbit_manager:top50(),
	send_response(Req, jiffy:encode(#{<<"result">> => lists:map(fun(E) -> gitbit_utils:to_json(E) end, Res), <<"total">> => length(Res)})).


