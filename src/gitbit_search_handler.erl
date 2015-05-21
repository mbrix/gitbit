-module(gitbit_search_handler).
-include("include/gitbit.hrl").

-export([init/2]).
-define(BODY_LIMIT, 1024).

init(Req, Opts) ->
	Method = cowboy_req:method(Req),
	Req2 = validate_req(Method, Req),
	{ok, Req2, Opts}.

validate_req(<<"POST">>, Req) ->
	recent(Req);
validate_req(_, Req) ->
	cowboy_req:reply(405, Req).

send_response(Req, Response) ->
	cowboy_req:reply(200, [
						   {<<"content-type">>, <<"application/json">>}
						  ], Response, Req).

recent(Req) ->
	send_response(Req, <<"Searched">>).
