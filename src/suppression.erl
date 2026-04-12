-module(suppression).
-export([handle/4, checkAuth/1]).

% Parses CLIENT_KEYS (semicolon-separated "name=value" pairs) into a set of valid tokens.
parseClientKeys(ClientKeysStr) ->
	Entries = string:tokens(ClientKeysStr, ";"),
	lists:foldl(
		fun (Entry, Acc) ->
			case string:split(Entry, "=", leading) of
				[_Name, Value] -> sets:add_element(Value, Acc);
				_ -> Acc
			end
		end,
		sets:new(),
		Entries
	).

% Checks the Authorization: Bearer header against CLIENT_KEYS env var.
% Returns ok if auth passes.
% Returns {error, unauthorized} if the token is wrong or missing.
checkAuth(Headers) ->
	ValidKeys = parseClientKeys(os:getenv("CLIENT_KEYS", "")),
	AuthHeader = maps:get('Authorization', Headers, ""),
	case AuthHeader of
		"Bearer " ++ Token ->
			case sets:is_element(Token, ValidKeys) of
				true -> ok;
				false -> {error, unauthorized}
			end;
		_ -> {error, unauthorized}
	end.


% Handles all /suppress/* routes. Returns {StatusCode, ContentType, Body}.
handle(Path, Method, Body, StatePid) ->
	case Path of
		"/suppress/clear" ->
			case Method of
				'POST' ->
					try jiffy:decode(list_to_binary(Body), [return_maps]) of
						#{<<"systemDeployed">> := System} ->
							gen_server:call(StatePid, {unsuppress, binary_to_list(System)}),
							{204, "text/plain", ""};
						_ ->
							{400, "text/plain", "Missing systemDeployed field"}
					catch
						_:_ ->
							{400, "text/plain", "Invalid JSON body"}
					end;
				_ ->
					{405, "text/plain", "Method Not Allowed"}
			end;
		_ ->
			case string:prefix(Path, "/suppress/") of
				nomatch ->
					nomatch;
				System ->
					case Method of
						'PUT' ->
							case gen_server:call(StatePid, {suppress, System}) of
								ok ->
									{204, "text/plain", ""};
								{error, not_found} ->
									{404, "text/plain", "System not found"}
							end;
						'DELETE' ->
							gen_server:call(StatePid, {unsuppress, System}),
							{204, "text/plain", ""};
						_ ->
							{405, "text/plain", "Method Not Allowed"}
					end
			end
	end.

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").

	checkAuth_no_client_keys_configured_test() ->
		% When CLIENT_KEYS is not set, all requests pass (no auth enforced)
		os:unsetenv("CLIENT_KEYS"),
		?assertEqual({error, unauthorized}, checkAuth(#{})),
		?assertEqual({error, unauthorized}, checkAuth(#{'Authorization' => "Bearer sometoken"})).

	checkAuth_valid_token_test() ->
		os:putenv("CLIENT_KEYS", "lucos_deploy_orb=mysecrettoken"),
		?assertEqual(ok, checkAuth(#{'Authorization' => "Bearer mysecrettoken"})),
		os:unsetenv("CLIENT_KEYS").

	checkAuth_missing_header_test() ->
		os:putenv("CLIENT_KEYS", "lucos_deploy_orb=mysecrettoken"),
		?assertEqual({error, unauthorized}, checkAuth(#{})),
		os:unsetenv("CLIENT_KEYS").

	checkAuth_wrong_token_test() ->
		os:putenv("CLIENT_KEYS", "lucos_deploy_orb=mysecrettoken"),
		?assertEqual({error, unauthorized}, checkAuth(#{'Authorization' => "Bearer wrongtoken"})),
		os:unsetenv("CLIENT_KEYS").

	checkAuth_no_bearer_prefix_test() ->
		os:putenv("CLIENT_KEYS", "lucos_deploy_orb=mysecrettoken"),
		?assertEqual({error, unauthorized}, checkAuth(#{'Authorization' => "mysecrettoken"})),
		os:unsetenv("CLIENT_KEYS").

	checkAuth_multiple_client_keys_test() ->
		% Multiple keys in CLIENT_KEYS — any valid key passes
		os:putenv("CLIENT_KEYS", "client_a=tokenA;client_b=tokenB"),
		?assertEqual(ok, checkAuth(#{'Authorization' => "Bearer tokenA"})),
		?assertEqual(ok, checkAuth(#{'Authorization' => "Bearer tokenB"})),
		?assertEqual({error, unauthorized}, checkAuth(#{'Authorization' => "Bearer tokenC"})),
		os:unsetenv("CLIENT_KEYS").

	checkAuth_token_with_equals_sign_test() ->
		% Token values containing = (e.g. base64-encoded) must be handled correctly —
		% parseClientKeys must split on the first = only, not every =
		os:putenv("CLIENT_KEYS", "lucos_deploy_orb=abc123=="),
		?assertEqual(ok, checkAuth(#{'Authorization' => "Bearer abc123=="})),
		?assertEqual({error, unauthorized}, checkAuth(#{'Authorization' => "Bearer abc123"})),
		os:unsetenv("CLIENT_KEYS").

-endif.
