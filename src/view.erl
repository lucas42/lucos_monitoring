-module(view).
-export([render_page/1, render_dashboard_block/1]).

% Renders the full HTML page for a list of systems.
% Reads the page shell from index.html, builds the dashboard block via render_dashboard_block/1,
% then substitutes <<TITLE>>, <<STYLESHEET_HREF>>, and <<BODY>> placeholders.
% The <<PLACEHOLDER>> syntax is safe because htmlEscape converts '<' to '&lt;',
% so user-supplied content can never contain '<<' after escaping.
render_page(Systems) ->
	{ok, Template} = file:read_file("index.html"),
	Block = render_dashboard_block(Systems),
	Page0 = binary:replace(Template, <<"<<TITLE>>">>, <<"Lucos Monitoring">>),
	Page1 = binary:replace(Page0, <<"<<STYLESHEET_HREF>>">>, <<"/style.css">>),
	Page2 = binary:replace(Page1, <<"<<BODY>>">>, list_to_binary(Block)),
	binary_to_list(Page2).

% Renders the dashboard block (the <div id="checks"> element and its contents)
% for a list of systems. Called by render_page/1 for the initial page load
% and by stream_handler when pushing SSE updates to connected clients.
render_dashboard_block(Systems) ->
	"<div id=\"checks\">\n" ++ renderAll(Systems) ++ "\n</div>".


formatStringFromInfo(Key, CheckInfo) ->
	formatString(Key, maps:get(Key, CheckInfo, <<"">>)).

% Escapes HTML special characters to prevent XSS. Must be applied to all
% external data before inserting into HTML. & must be escaped first to avoid
% double-escaping the entities introduced by subsequent replacements.
htmlEscape(Value) ->
	lists:foldl(
		fun ({From, To}, Acc) ->
			re:replace(Acc, From, To, [global, {return, list}])
		end,
		Value,
		[{"&", "\\&amp;"}, {"<", "\\&lt;"}, {">", "\\&gt;"}, {"\"", "\\&quot;"}]
	).

formatString(Key, BinaryValue) ->
	RawValue = binary_to_list(BinaryValue),
	EscapedValue = htmlEscape(RawValue),
	ZWSValue = re:replace(EscapedValue, "_", "\\&ZeroWidthSpace;_",[global, {return, list}]),
	LinkedValue = re:replace(ZWSValue, "https?:\\S+", "<a href=\"&\" target=\"_blank\">&</a>",[global, {return, list}]),
	Value = re:replace(LinkedValue, "href=\"([^\"]*)&ZeroWidthSpace;([^\"]*)\"", "href=\"\\g1\\g2\"",[global, {return, list}]),
	"<td class=\"formattedString "++binary_to_list(Key)++"\">"++Value++"</td>\r\n".

% Maps a check status atom to a sort priority (lower = first in table).
checkStatusSortPriority(failing)   -> 0;
checkStatusSortPriority(unknown)   -> 1;
checkStatusSortPriority(buffering) -> 2;
checkStatusSortPriority(_)         -> 3. % healthy (and any future statuses)

% Maps a system status atom to a sort priority (lower = first in page).
systemStatusSortPriority(failing)              -> 0;
systemStatusSortPriority(unknown)              -> 1;
systemStatusSortPriority(buffering)            -> 2;
systemStatusSortPriority(suppressed)           -> 3;
systemStatusSortPriority(pending_verification) -> 3;
systemStatusSortPriority(_)                    -> 4. % healthy

% Maps a status atom to a CSS class string.
% pending_verification uses a hyphen to match existing CSS convention.
statusToCssClass(pending_verification) -> "pending-verification";
statusToCssClass(Status) -> atom_to_list(Status).

renderSystemChecks(SystemChecks) ->
	SortedChecks = lists:sort(
		fun (CheckA, CheckB) ->
			StatusA = maps:get(<<"status">>, CheckA, unknown),
			StatusB = maps:get(<<"status">>, CheckB, unknown),
			IdA = maps:get(<<"id">>, CheckA, <<>>),
			IdB = maps:get(<<"id">>, CheckB, <<>>),
			{checkStatusSortPriority(StatusA), IdA} =< {checkStatusSortPriority(StatusB), IdB}
		end, SystemChecks),
	"<div  class=\"system-checks\"><table>
		<thead><td>Check</td><td>Status</td><td>Technical Detail</td><td class=\"debug\">Debug</td></thead>
		" ++ lists:foldl(
		fun (Check, Html) ->
			CheckId = maps:get(<<"id">>, Check),
			Status = maps:get(<<"status">>, Check, unknown),
			StatusText = binary_to_list(maps:get(<<"statusText">>, Check, <<"unknown">>)),
			Link = binary_to_list(maps:get(<<"link">>, Check, <<"">>)),
			CheckClass = "check " ++ statusToCssClass(Status),
			StatusHtml = case Link of
				"" -> StatusText;
				_ -> "<a href=\""++Link++"\" target=\"_blank\">"++StatusText++"</a>"
			end,
			CheckHtml = "
				<tr class=\""++CheckClass++"\">
					"++formatString(<<"checkid">>, CheckId)++"
					<td class=\"status\">"++StatusHtml++"</td>
					"++formatStringFromInfo(<<"techDetail">>, Check)
					++formatStringFromInfo(<<"debug">>, Check)
				++"</tr>
			",
			Html++CheckHtml
		end, "", SortedChecks) ++ "
	</table></div>".


renderSystemMetrics(SystemMetrics) ->
	SortedMetrics = lists:sort(
		fun (MetricA, MetricB) ->
			maps:get(<<"id">>, MetricA) =< maps:get(<<"id">>, MetricB)
		end, SystemMetrics),
	Html = lists:foldl(
		fun (Metric, Html) ->
			MetricId = binary_to_list(maps:get(<<"id">>, Metric)),
			Value = maps:get(<<"value">>, Metric, -1),
			MetricHtml = lists:flatten(
				"<tr class=\"metric\">"
				++ "<td class=\"metricid\">" ++ MetricId ++ "</td>"
				++ "<td class=\"value\">" ++ lists:flatten(io_lib:format("~p", [Value])) ++ "</td>"
				++ formatStringFromInfo(<<"techDetail">>, Metric)
				++ "</tr>"
			),
			Html++MetricHtml
		end, "", SortedMetrics),
	case Html of
		"" -> "";
		_ ->
			"
			<table class=\"metrics\">
				<thead><td>Metric</td><td>Value</td><td>Technical Detail</td></thead>
				"++Html++"
			</table>"
	end.

renderSystemHeader(Name, Host, DupNameCount) ->
	SystemId = binary_to_list(Name),
	ReadableName = re:replace(SystemId, "_", " ", [global, {return,list}]),
	InfoLinkHtml = case Host of
		"" -> "";
		_ ->
			InfoURL = "https://" ++ Host ++ "/_info",
			"<a href=\""++InfoURL++"\" target=\"_blank\" class=\"rawInfoURL\">&#128279;</a>\n\t\t\t\t"
	end,
	Disambiguator = case Host of "" -> SystemId; _ -> Host end,
	case DupNameCount of
		1 ->
			"<h2 id=\"system-"++SystemId++"\">\n\t\t\t\t"++InfoLinkHtml++
			"<span class=\"system-name\">"++ReadableName++"</span>\n\t\t\t</h2>";
		_ ->
			"<h2 id=\"system-"++SystemId++"\">\n\t\t\t\t"++InfoLinkHtml++
			"<span class=\"system-name\">"++ReadableName++"</span> ("++Disambiguator++")\n\t\t\t</h2>"
	end.

% Renders all systems. Systems is the list returned by {fetch, all} — each
% element is a map with <<"host">>, <<"name">>, <<"status">>, <<"checks">>, <<"metrics">>.
% Status-based CSS class and suppression states are derived from <<"status">> directly;
% no separate suppression map fetch is needed.
renderAll(Systems) ->
	SortedSystems = lists:sort(
		fun (SysA, SysB) ->
			StatusA = maps:get(<<"status">>, SysA),
			StatusB = maps:get(<<"status">>, SysB),
			NameA = maps:get(<<"name">>, SysA, <<"">>),
			NameB = maps:get(<<"name">>, SysB, <<"">>),
			HostA = maps:get(<<"host">>, SysA, <<"">>),
			HostB = maps:get(<<"host">>, SysB, <<"">>),
			{systemStatusSortPriority(StatusA), NameA, HostA} =< {systemStatusSortPriority(StatusB), NameB, HostB}
		end, Systems),
	lists:foldl(
		fun (System, Output) ->
			Name = maps:get(<<"name">>, System),
			Host = binary_to_list(maps:get(<<"host">>, System, <<"">>)),
			Status = maps:get(<<"status">>, System),
			SystemChecks = maps:get(<<"checks">>, System, []),
			SystemMetrics = maps:get(<<"metrics">>, System, []),
			DupNameCount = length(lists:filter(
				fun(S) -> maps:get(<<"name">>, S, <<>>) =:= Name end,
				Systems)),
			CssClass = "system " ++ statusToCssClass(Status),
			Output++"
			<div class=\""++CssClass++"\">
				"++renderSystemHeader(Name, Host, DupNameCount)++"
				"++renderSystemChecks(SystemChecks)++"
				"++renderSystemMetrics(SystemMetrics)++"
			</div>
			"
		end, "", SortedSystems).

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").

	checkStatusSortPriority_failing_test() ->
		?assertEqual(0, checkStatusSortPriority(failing)).

	checkStatusSortPriority_unknown_test() ->
		?assertEqual(1, checkStatusSortPriority(unknown)).

	checkStatusSortPriority_buffering_test() ->
		?assertEqual(2, checkStatusSortPriority(buffering)).

	checkStatusSortPriority_healthy_test() ->
		?assertEqual(3, checkStatusSortPriority(healthy)).

	systemStatusSortPriority_failing_test() ->
		?assertEqual(0, systemStatusSortPriority(failing)).

	systemStatusSortPriority_unknown_test() ->
		?assertEqual(1, systemStatusSortPriority(unknown)).

	systemStatusSortPriority_buffering_test() ->
		?assertEqual(2, systemStatusSortPriority(buffering)).

	systemStatusSortPriority_suppressed_test() ->
		?assertEqual(3, systemStatusSortPriority(suppressed)).

	systemStatusSortPriority_pending_verification_test() ->
		?assertEqual(3, systemStatusSortPriority(pending_verification)).

	systemStatusSortPriority_healthy_test() ->
		?assertEqual(4, systemStatusSortPriority(healthy)).

	statusToCssClass_healthy_test() ->
		?assertEqual("healthy", statusToCssClass(healthy)).

	statusToCssClass_failing_test() ->
		?assertEqual("failing", statusToCssClass(failing)).

	statusToCssClass_unknown_test() ->
		?assertEqual("unknown", statusToCssClass(unknown)).

	statusToCssClass_buffering_test() ->
		?assertEqual("buffering", statusToCssClass(buffering)).

	statusToCssClass_pending_verification_test() ->
		?assertEqual("pending-verification", statusToCssClass(pending_verification)).

	statusToCssClass_suppressed_test() ->
		?assertEqual("suppressed", statusToCssClass(suppressed)).

	renderSystemChecks_order_test() ->
		SystemChecks = [
			#{<<"id">> => <<"b-healthy">>,   <<"status">> => healthy,   <<"statusText">> => <<"healthy">>},
			#{<<"id">> => <<"a-failing">>,   <<"status">> => failing,   <<"statusText">> => <<"failing">>},
			#{<<"id">> => <<"c-unknown">>,   <<"status">> => unknown,   <<"statusText">> => <<"unknown">>},
			#{<<"id">> => <<"d-buffering">>, <<"status">> => buffering, <<"statusText">> => <<"unknown (1)">>},
			#{<<"id">> => <<"a-healthy">>,   <<"status">> => healthy,   <<"statusText">> => <<"healthy">>}
		],
		Html = renderSystemChecks(SystemChecks),
		PosFailing   = string:str(Html, "a-failing"),
		PosUnknown   = string:str(Html, "c-unknown"),
		PosBuffering = string:str(Html, "d-buffering"),
		PosAHealthy  = string:str(Html, "a-healthy"),
		PosBHealthy  = string:str(Html, "b-healthy"),
		?assert(PosFailing   < PosUnknown,   "failing must come before unknown"),
		?assert(PosUnknown   < PosBuffering, "unknown must come before buffering"),
		?assert(PosBuffering < PosAHealthy,  "buffering must come before healthy"),
		?assert(PosAHealthy  < PosBHealthy,  "healthy checks must be sorted alphabetically").

	renderSystemMetrics_order_test() ->
		SystemMetrics = [
			#{<<"id">> => <<"z-metric">>, <<"value">> => 1, <<"techDetail">> => <<"">>},
			#{<<"id">> => <<"a-metric">>, <<"value">> => 2, <<"techDetail">> => <<"">>},
			#{<<"id">> => <<"m-metric">>, <<"value">> => 3, <<"techDetail">> => <<"">>}
		],
		Html = renderSystemMetrics(SystemMetrics),
		PosA = string:str(Html, "a-metric"),
		PosM = string:str(Html, "m-metric"),
		PosZ = string:str(Html, "z-metric"),
		?assert(PosA < PosM, "a-metric must come before m-metric"),
		?assert(PosM < PosZ, "m-metric must come before z-metric").

	% Regression test: non-ASCII bytes in techDetail (e.g. em-dash U+2014, UTF-8 bytes 226,128,148)
	% must NOT appear as Erlang integer-list form ([226,128,148,...]) or binary form (<<226,...>>).
	renderSystemMetrics_nonascii_techdetail_test() ->
		SystemMetrics = [
			#{<<"id">> => <<"test-metric">>, <<"value">> => 42, <<"techDetail">> => <<"Count \xe2\x80\x94 current total">>}
		],
		Html = renderSystemMetrics(SystemMetrics),
		?assertEqual(0, string:str(Html, "226,128,148"), "techDetail must not appear as integer-list form"),
		?assertEqual(0, string:str(Html, "<<226"), "techDetail must not appear as binary term form"),
		?assert(string:str(Html, "test-metric") > 0, "metric ID must appear in HTML").

	htmlEscape_no_special_chars_test() ->
		?assertEqual("hello world", htmlEscape("hello world")).

	htmlEscape_ampersand_test() ->
		?assertEqual("foo &amp; bar", htmlEscape("foo & bar")).

	htmlEscape_angle_brackets_test() ->
		?assertEqual("&lt;script&gt;alert(1)&lt;/script&gt;", htmlEscape("<script>alert(1)</script>")).

	htmlEscape_double_quote_test() ->
		?assertEqual("say &quot;hello&quot;", htmlEscape("say \"hello\"")).

	htmlEscape_all_special_chars_test() ->
		?assertEqual("&lt;a href=&quot;x&quot;&gt;foo &amp; bar&lt;/a&gt;", htmlEscape("<a href=\"x\">foo & bar</a>")).

	htmlEscape_no_double_encoding_test() ->
		% Escaping once should not double-encode on a second pass
		Escaped = htmlEscape("a & b"),
		?assertEqual("a &amp; b", Escaped),
		?assertEqual("a &amp;amp; b", htmlEscape(Escaped)).

	formatString_escapes_html_test() ->
		% A techDetail containing a script tag should be escaped, not executed
		Result = formatString(<<"techDetail">>, <<"<script>alert(1)</script>">>),
		?assertEqual(false, string:str(Result, "<script>") > 0),
		?assert(string:str(Result, "&lt;script&gt;") > 0).

	formatString_url_still_linkified_test() ->
		% A plain URL in a value should still be wrapped in an anchor tag
		Result = formatString(<<"debug">>, <<"See https://example.com/path for details">>),
		?assert(string:str(Result, "<a href=") > 0),
		?assert(string:str(Result, "https://example.com/path") > 0).

	render_dashboard_block_has_checks_div_test() ->
		Systems = [],
		Html = render_dashboard_block(Systems),
		?assert(string:str(Html, "<div id=\"checks\">") > 0),
		?assert(string:str(Html, "</div>") > 0).

	render_dashboard_block_contains_system_test() ->
		Systems = [#{
			<<"host">> => <<"example.l42.eu">>,
			<<"name">> => <<"lucos_example">>,
			<<"id">> => <<"lucos_example">>,
			<<"status">> => healthy,
			<<"checks">> => [],
			<<"metrics">> => []
		}],
		Html = render_dashboard_block(Systems),
		?assert(string:str(Html, "<div id=\"checks\">") > 0),
		?assert(string:str(Html, "lucos example") > 0),
		?assert(string:str(Html, "id=\"system-lucos_example\"") > 0),
		?assert(string:str(Html, "rawInfoURL") > 0).

	render_dashboard_block_no_host_test() ->
		% Systems without a host (components) should not show a rawInfoURL link
		Systems = [#{
			<<"host">> => <<"">>,
			<<"name">> => <<"lucos_component">>,
			<<"id">> => <<"lucos_component">>,
			<<"status">> => healthy,
			<<"checks">> => [],
			<<"metrics">> => []
		}],
		Html = render_dashboard_block(Systems),
		?assert(string:str(Html, "id=\"system-lucos_component\"") > 0),
		?assertEqual(0, string:str(Html, "rawInfoURL")).

-endif.
