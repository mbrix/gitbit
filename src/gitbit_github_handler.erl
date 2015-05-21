-module(gitbit_github_handler).
-include("include/gitbit.hrl").

-export([init/2]).

-export([checksum/1]).

-define(BODY_LIMIT, 1024*1024).

init(Req, Opts) ->
	Method = cowboy_req:method(Req),
	HasBody = cowboy_req:has_body(Req),
	Agent = cowboy_req:header(<<"user-agent">>, Req),
	Event = cowboy_req:header(<<"x-github-event">>, Req),
	Delivery = cowboy_req:header(<<"x-github-delivery">>, Req),
	lager:info("Request: ~p ~p ~p", [Agent, Event, Delivery]),
	Req2 = validate_req(Method, HasBody, Req),
	{ok, Req2, Opts}.

validate_req(<<"POST">>, true, Req) ->
	{_, Body, Req2} = cowboy_req:body(Req, [{length, ?BODY_LIMIT}]),
	PostData = jiffy:decode(Body, [return_maps]),
	%try
	parse_bounties(maps:get(<<"commits">>, PostData),
				   maps:get(<<"repository">>, PostData)),
	send_response(Req2);
	%catch
	%	_:_ -> 	return_error(<<"InvalidArgument">>, Req)
	%end;

validate_req(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], <<"Missing body.">>, Req);
validate_req(_, _, Req) ->
	cowboy_req:reply(405, Req).

send_response(Req) ->
	Result = <<"Parsed Bounties">>,
	cowboy_req:reply(200, [
						   {<<"content-type">>, <<"application/json">>}
						  ], Result, Req).

parse_bounties(Commits, Repository) ->
	RepoName = maps:get(<<"full_name">>, Repository),
	{ok, BountyRegex} = re:compile(<<"^gitbit-bounty![ \t]*?\\[([^ \t]+)\\](.*)">>,
							 [multiline]),
	{ok, ClaimRegex} = re:compile(<<"^gitbit-claim![ \t]*?\\[([^ \t]+)\\][ \t]*?([^ \t]+)(.*)">>,
								   [multiline]),

	parse_commits(Commits, RepoName, BountyRegex, ClaimRegex).

parse_commits([], _, _, _) -> ok;
parse_commits([Commit|T], RepoName, BR, CR) ->
	Hash = maps:get(<<"id">>, Commit),
	Message = maps:get(<<"message">>, Commit),
	Url = maps:get(<<"url">>, Commit),
	Checksum = crypto:hash(sha256, Message),
	%% Recursively scan message
	Bounties = scan_msgs(Message, bounty, BR),
	Claims = scan_msgs(Message, claim, CR),
	%% Remove bounties with corresponding claims in same commit
	MergedClaims = lists:ukeymerge(2, Bounties, Claims),
	Verified = verify_claims(Checksum, Hash, RepoName),
	add_bounties(Verified, MergedClaims, RepoName, Url),
	parse_commits(T, RepoName, BR, CR).

scan_msgs(Binstr, Tag, Regex) -> scan_msgs(Binstr, Tag, Regex, []).

scan_msgs(<<>>, _, _, Acc) -> lists:ukeysort(2, Acc);
scan_msgs(Binstr, Tag, Regex, Acc) ->
	case re:run(Binstr, Regex) of
		nomatch -> lists:ukeysort(2, Acc);
		{match,[{T1,TL1},{M1,L1},{M2,L2}]} ->
			Name = binary:part(Binstr, M1, L1),
			Desc = binary:part(Binstr, M2, L2),
			Size = T1*8+TL1*8,
			<<_:Size, Rest/bitstring>> = Binstr,
			scan_msgs(Rest, Tag, Regex, [{Tag, Name, Desc}|Acc]);
		{match,[{T1,TL1},{M1,L1},{M2,L2},{M3,L3}]} ->
			Name = binary:part(Binstr, M1, L1),
			Address = binary:part(Binstr, M2, L2),
			Desc = binary:part(Binstr, M3, L3),
			Size = T1*8+TL1*8,
			<<_:Size, Rest/bitstring>> = Binstr,
			case check_address(Address) of
				true -> scan_msgs(Rest, Tag, Regex, [{Tag, Name, Address, Desc}|Acc]);
				false -> scan_msgs(Rest, Tag, Regex, [invalid|Acc])
			end
	end.

check_address(Address) ->
	checksum(erlang:binary_to_list(Address)).

checksum(Address) ->
    <<ChecksumR:32/bitstring, R/binary>> = bin_reverse(base58:base58_to_binary(Address)),
    <<Checksum:32/bitstring, _/binary>> = crypto:hash(sha256, crypto:hash(sha256, bin_reverse(R))),
    Checksum =:= bin_reverse(ChecksumR).

bin_reverse(B) ->
   S = size(B)*8,
   <<X:S/integer-little>>=B,
   <<X:S/integer-big>>.

add_bounties(false,_,_,_) -> ok;
add_bounties(true, [], _, _) -> ok;
add_bounties(true, [{bounty, Name, ShortDesc}|T], RepoName, Url) ->
	gitbit_manager:new_bounty(Name, RepoName, ShortDesc, Url),
	add_bounties(true, T, RepoName, Url);
add_bounties(true, [{claim, Name, Address, ShortBrag}|T], RepoName, Url) ->
	gitbit_manager:complete_bounty(Name, RepoName, Address, ShortBrag),
	add_bounties(true, T, RepoName, Url);
add_bounties(true, [invalid|T], RepoName, Url) ->
	add_bounties(true, T, RepoName, Url).

verify_claims(Checksum, Hash, Name) ->
	%% Use the github API to grab the raw commit message
	%% and checksum against the webhook data
	URL = <<"https://api.github.com/repos/", Name/bitstring, "/git/commits/", Hash/bitstring>>,
	Headers = [{<<"User-Agent">>, <<"Gitbit-bounty">>}],
	Payload = <<>>,
	Options = [],
	{ok, _StatusCode, _RespHeaders, ClientRef} = hackney:request(get, URL,
                                                        Headers, Payload,
                                                        Options),
    {ok, Body} = hackney:body(ClientRef),
    DecodedData = jiffy:decode(Body, [return_maps]),
    Message = maps:get(<<"message">>, DecodedData),
    Checksum =:= crypto:hash(sha256, Message).
