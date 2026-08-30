-module(view).
-export([render_page/1, render_dashboard_block/1]).

% Threshold in seconds beyond which a data source is considered stale.
% At a 60-second poll interval, 300 seconds = 5 missed polls before warning.
-define(STALE_THRESHOLD_SECS, 300).

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
% Classifications (#296) is computed once here and threaded through both the summary
% rollup line and the per-system rendering, so the two can never disagree.
render_dashboard_block(Systems) ->
	Classifications = classifyFailures(Systems),
	"<div id=\"checks\">\n" ++ renderFailureRollup(Classifications) ++ renderAll(Systems, Classifications) ++ "\n</div>".

% Returns the system-id strings (as Erlang lists) of every currently-`failing` system.
failingSystemIds(Systems) ->
	sets:from_list(
		[binary_to_list(maps:get(<<"name">>, S)) || S <- Systems, maps:get(<<"status">>, S) =:= failing],
		[{version, 2}]
	).

% Returns the normalised dependsOn system IDs declared by ANY check belonging to this
% system — not just whichever check happens to be failing right now. This matters: the
% check that fails first during a dependency outage can be monitoring's own reachability
% probe (fetch-info), which carries no dependsOn on the upstream system at fault — that
% declaration lives on a different check, one which may never have run this poll because
% fetch-info failed first. See #296 and ADR-0004.
systemDependsOn(SystemChecks) ->
	lists:usort(lists:append([
		[binary_to_list(D) || D <- maps:get(<<"dependsOn">>, Check, [])] || Check <- SystemChecks
	])).

% Classifies one failing system as `root` or `{dependent, RootSystemId}`: dependent when
% any system it declares a dependency on (via systemDependsOn/1) is ALSO currently
% failing. Single-hop only (ADR-0002/0004) — a dependent's own dependents are not chased,
% matching #296's explicit exclusion of a dependency-graph view. Self-references are
% guarded defensively even though no declared dependsOn in this estate is reflexive. When
% more than one declared dependency is itself failing, the first after sorting is
% reported as the anchor.
classifyFailure(SystemId, SystemChecks, FailingIds) ->
	DependsOn = systemDependsOn(SystemChecks),
	FailingDeps = [D || D <- DependsOn, D =/= SystemId, sets:is_element(D, FailingIds)],
	case FailingDeps of
		[] -> root;
		[RootId | _] -> {dependent, RootId}
	end.

% Builds a map of SystemId -> root | {dependent, RootSystemId} for every currently-failing
% system. Systems in any other status are absent — the root/dependent distinction only
% means something for a system that is actually failing.
classifyFailures(Systems) ->
	FailingIds = failingSystemIds(Systems),
	lists:foldl(fun(S, Acc) ->
		case maps:get(<<"status">>, S) of
			failing ->
				SystemId = binary_to_list(maps:get(<<"name">>, S)),
				Checks = maps:get(<<"checks">>, S, []),
				maps:put(SystemId, classifyFailure(SystemId, Checks, FailingIds), Acc);
			_ -> Acc
		end
	end, #{}, Systems).

% Sort key that clusters a dependent system immediately after its root cause, rather than
% floating free among unrelated failures purely by name (#296) — a dependent's whole point
% is to read next to the system it's explained by. Non-dependent systems (including every
% healthy/unknown/etc. system, which classifyFailures/1 never classifies) group under
% their own ID and sort first within any shared group.
sortGroupKey(SystemId, Classifications) ->
	case maps:get(SystemId, Classifications, root) of
		{dependent, RootId} -> {RootId, true};
		_ -> {SystemId, false}
	end.

% Renders the "N failing — M root causes (...), K dependent" summary line, or "" when
% grouping wouldn't change how the failures read: either nothing is classified dependent,
% or (degenerate — only reachable via a dependsOn cycle, which nothing in this estate's
% declared dependencies forms) every failing system is dependent with no anchor to name.
% A rollup claiming "0 root causes" would be worse than no rollup at all.
renderFailureRollup(Classifications) ->
	Entries = maps:to_list(Classifications),
	RootIds = lists:usort([Id || {Id, root} <- Entries]),
	DependentCount = length([ok || {_, {dependent, _}} <- Entries]),
	FailingCount = length(Entries),
	case {DependentCount, RootIds} of
		{0, _} -> "";
		{_, []} -> "";
		_ ->
			RootLinksHtml = string:join([rootLink(Id) || Id <- RootIds], ", "),
			RootWord = case length(RootIds) of 1 -> "root cause"; _ -> "root causes" end,
			"<p class=\"failure-rollup\">"
			++ integer_to_list(FailingCount) ++ " failing &mdash; "
			++ integer_to_list(length(RootIds)) ++ " " ++ RootWord ++ " (" ++ RootLinksHtml ++ "), "
			++ integer_to_list(DependentCount) ++ " dependent</p>\n"
	end.

rootLink(SystemId) ->
	"<a href=\"#system-"++SystemId++"\">"++readableName(SystemId)++"</a>".

% Dependent-failure note rendered under a dependent system's header (#296). Text, not
% colour alone, carries the meaning — matches the freshness-indicator convention above.
% "Never hidden": this supplements the system's normal failing display, it never replaces it.
renderDependencyNote(RootId) ->
	"<p class=\"dependency-note\">Dependent on <a href=\"#system-"++RootId++"\">"++readableName(RootId)++"</a></p>".

% Converts a system ID (e.g. "lucos_time") to its human-readable form ("lucos time").
readableName(SystemId) ->
	re:replace(SystemId, "_", " ", [global, {return, list}]).


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

% Maps a configy type atom to {EmojiHtml, TypeLabel}.
% EmojiHtml is an HTML entity string; TypeLabel is a human-readable type name
% used for aria-label and title attributes on the icon span.
typeToEmoji(system)    -> {"&#127961;&#65039;", "System"};     % 🏙️
typeToEmoji(host)      -> {"&#128421;&#65039;", "Host"};       % 🖥️
typeToEmoji(component) -> {"&#128451;&#65039;", "Component"};  % 🗃️
typeToEmoji(_)         -> {"&#10068;", "Unknown type"}.        % ❔

% Returns a human-readable string for a duration in seconds.
% Examples: "just now", "45s ago", "3m ago", "1h ago", "7h 20m ago".
formatAge(0) -> "just now";
formatAge(AgeSecs) when AgeSecs < 60 ->
	integer_to_list(AgeSecs) ++ "s ago";
formatAge(AgeSecs) when AgeSecs < 3600 ->
	Mins = AgeSecs div 60,
	integer_to_list(Mins) ++ "m ago";
formatAge(AgeSecs) ->
	Hours = AgeSecs div 3600,
	Mins = (AgeSecs rem 3600) div 60,
	case Mins of
		0 -> integer_to_list(Hours) ++ "h ago";
		_ -> integer_to_list(Hours) ++ "h " ++ integer_to_list(Mins) ++ "m ago"
	end.

% Renders a freshness indicator <p> element for a system section.
% LastUpdated is the most recent source timestamp (Unix seconds).
% OldestSourceTs is the oldest source timestamp (Unix seconds).
% Returns "" when no timestamps are available (brand-new system before first render).
% When OldestSourceTs is stale (>STALE_THRESHOLD_SECS), renders a warning that
% uses both an icon AND text — the distinction does not rely on colour alone.
renderFreshnessIndicator(0, _) -> "";
renderFreshnessIndicator(LastUpdated, OldestSourceTs) ->
	Now = erlang:system_time(second),
	MostRecentAge = max(0, Now - LastUpdated),
	OldestAge = max(0, Now - OldestSourceTs),
	DataAttrs = "data-last-updated=\"" ++ integer_to_list(LastUpdated)
	         ++ "\" data-oldest-source-ts=\"" ++ integer_to_list(OldestSourceTs) ++ "\"",
	case OldestAge > ?STALE_THRESHOLD_SECS of
		true ->
			AgeStr = formatAge(OldestAge),
			"<p class=\"freshness-indicator stale\" " ++ DataAttrs ++ ">"
			++ "<span aria-hidden=\"true\">&#9888;&#65039;</span>"
			++ " Data may be outdated &mdash; last received " ++ AgeStr
			++ "</p>";
		false ->
			"<p class=\"freshness-indicator\" " ++ DataAttrs ++ ">"
			++ "Updated " ++ formatAge(MostRecentAge)
			++ "</p>"
	end.

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

renderSystemHeader(Name, Host, Type, DupNameCount) ->
	SystemId = binary_to_list(Name),
	ReadableName = readableName(SystemId),
	{Emoji, TypeLabel} = typeToEmoji(Type),
	EmojiHtml = "<span class=\"type-icon\" role=\"img\" aria-label=\""++TypeLabel++"\" title=\""++TypeLabel++"\">"++Emoji++"</span>\n\t\t\t\t",
	InfoLinkHtml = case Host of
		"" -> "";
		_ ->
			InfoURL = "https://" ++ Host ++ "/_info",
			"<a href=\""++InfoURL++"\" target=\"_blank\" class=\"rawInfoURL\">&#128279;</a>\n\t\t\t\t"
	end,
	Disambiguator = case Host of "" -> SystemId; _ -> Host end,
	case DupNameCount of
		1 ->
			"<h2 id=\"system-"++SystemId++"\">\n\t\t\t\t"++EmojiHtml++InfoLinkHtml++
			"<span class=\"system-name\">"++ReadableName++"</span>\n\t\t\t</h2>";
		_ ->
			"<h2 id=\"system-"++SystemId++"\">\n\t\t\t\t"++EmojiHtml++InfoLinkHtml++
			"<span class=\"system-name\">"++ReadableName++"</span> ("++Disambiguator++")\n\t\t\t</h2>"
	end.

% Renders all systems. Systems is the list returned by {fetch, all} — each
% element is a map with <<"host">>, <<"name">>, <<"status">>, <<"checks">>, <<"metrics">>.
% Status-based CSS class and suppression states are derived from <<"status">> directly;
% no separate suppression map fetch is needed. Classifications is the map built by
% classifyFailures/1 — passed in rather than recomputed here so the per-system rendering
% can never disagree with the rollup line built from the same map (#296).
renderAll(Systems, Classifications) ->
	SortedSystems = lists:sort(
		fun (SysA, SysB) ->
			StatusA = maps:get(<<"status">>, SysA),
			StatusB = maps:get(<<"status">>, SysB),
			NameA = maps:get(<<"name">>, SysA, <<"">>),
			NameB = maps:get(<<"name">>, SysB, <<"">>),
			HostA = maps:get(<<"host">>, SysA, <<"">>),
			HostB = maps:get(<<"host">>, SysB, <<"">>),
			{GroupA, DependentA} = sortGroupKey(binary_to_list(NameA), Classifications),
			{GroupB, DependentB} = sortGroupKey(binary_to_list(NameB), Classifications),
			{systemStatusSortPriority(StatusA), GroupA, DependentA, NameA, HostA}
				=< {systemStatusSortPriority(StatusB), GroupB, DependentB, NameB, HostB}
		end, Systems),
	lists:foldl(
		fun (System, Output) ->
			Name = maps:get(<<"name">>, System),
			SystemId = binary_to_list(Name),
			Host = binary_to_list(maps:get(<<"host">>, System, <<"">>)),
			Status = maps:get(<<"status">>, System),
			Type = maps:get(<<"type">>, System, unknown),
			SystemChecks = maps:get(<<"checks">>, System, []),
			SystemMetrics = maps:get(<<"metrics">>, System, []),
			LastUpdated = maps:get(<<"last_updated">>, System, 0),
			OldestSourceTs = maps:get(<<"oldest_source_ts">>, System, 0),
			DupNameCount = length(lists:filter(
				fun(S) -> maps:get(<<"name">>, S, <<>>) =:= Name end,
				Systems)),
			{CssClass, DependencyNoteHtml} = case maps:get(SystemId, Classifications, root) of
				{dependent, RootId} -> {"system " ++ statusToCssClass(Status) ++ " dependent-failure", renderDependencyNote(RootId)};
				_ -> {"system " ++ statusToCssClass(Status), ""}
			end,
			Output++"
			<div class=\""++CssClass++"\">
				"++renderSystemHeader(Name, Host, Type, DupNameCount)++"
				"++DependencyNoteHtml++"
				"++renderFreshnessIndicator(LastUpdated, OldestSourceTs)++"
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

	typeToEmoji_system_test() ->
		{Emoji, Label} = typeToEmoji(system),
		?assert(string:str(Emoji, "&#127961;") > 0, "system must use cityscape emoji (U+1F3D9)"),
		?assertEqual("System", Label).

	typeToEmoji_host_test() ->
		{Emoji, Label} = typeToEmoji(host),
		?assert(string:str(Emoji, "&#128421;") > 0, "host must use desktop computer emoji (U+1F5A5)"),
		?assertEqual("Host", Label).

	typeToEmoji_component_test() ->
		{Emoji, Label} = typeToEmoji(component),
		?assert(string:str(Emoji, "&#128451;") > 0, "component must use card file box emoji (U+1F5C3)"),
		?assertEqual("Component", Label).

	typeToEmoji_unknown_type_test() ->
		{Emoji, Label} = typeToEmoji(not_a_real_type),
		?assert(string:str(Emoji, "&#10068;") > 0, "unknown type must use question mark emoji (U+2754)"),
		?assertEqual("Unknown type", Label).

	render_dashboard_block_shows_type_icon_test() ->
		% A system with a known type should show a labelled type-icon span in the heading.
		Systems = [#{
			<<"host">> => <<"example.l42.eu">>,
			<<"name">> => <<"lucos_example">>,
			<<"id">> => <<"lucos_example">>,
			<<"status">> => healthy,
			<<"type">> => system,
			<<"checks">> => [],
			<<"metrics">> => []
		}],
		Html = render_dashboard_block(Systems),
		?assert(string:str(Html, "type-icon") > 0, "heading must include a type-icon span"),
		?assert(string:str(Html, "System") > 0, "type-icon must be labelled with the type name").

	render_dashboard_block_unknown_type_fallback_test() ->
		% A system with no type field should fall back to the unknown-type icon.
		Systems = [#{
			<<"host">> => <<"example.l42.eu">>,
			<<"name">> => <<"lucos_example">>,
			<<"id">> => <<"lucos_example">>,
			<<"status">> => healthy,
			<<"checks">> => [],
			<<"metrics">> => []
		}],
		Html = render_dashboard_block(Systems),
		?assert(string:str(Html, "type-icon") > 0, "heading must include a type-icon span even without a type"),
		?assert(string:str(Html, "Unknown type") > 0, "missing type must fall back to unknown-type label").

	% ── formatAge ────────────────────────────────────────────────────────────

	format_age_zero_test() ->
		?assertEqual("just now", formatAge(0)).

	format_age_seconds_test() ->
		?assertEqual("45s ago", formatAge(45)).

	format_age_just_under_minute_test() ->
		?assertEqual("59s ago", formatAge(59)).

	format_age_minutes_test() ->
		?assertEqual("3m ago", formatAge(180)).

	format_age_just_under_hour_test() ->
		?assertEqual("59m ago", formatAge(3540)).

	format_age_hours_exact_test() ->
		?assertEqual("2h ago", formatAge(7200)).

	format_age_hours_and_minutes_test() ->
		?assertEqual("7h 20m ago", formatAge(26400)).

	% ── renderFreshnessIndicator ─────────────────────────────────────────────

	render_freshness_indicator_no_timestamp_test() ->
		% When LastUpdated is 0, return empty string (no indicator yet).
		?assertEqual("", renderFreshnessIndicator(0, 0)).

	render_freshness_indicator_fresh_test() ->
		% When all sources are fresh, shows "Updated Xs ago" with no stale class.
		Now = erlang:system_time(second),
		Html = renderFreshnessIndicator(Now - 30, Now - 30),
		?assert(string:str(Html, "freshness-indicator") > 0, "must have freshness-indicator class"),
		?assertEqual(0, string:str(Html, "stale"), "must NOT have stale class when data is fresh"),
		?assert(string:str(Html, "Updated") > 0, "must show Updated prefix when fresh"),
		?assert(string:str(Html, "data-last-updated") > 0, "must include data-last-updated attribute").

	render_freshness_indicator_stale_test() ->
		% When oldest source is beyond threshold, shows warning with icon and text.
		Now = erlang:system_time(second),
		StaleTs = Now - 26400, % 7h 20m ago
		Html = renderFreshnessIndicator(Now - 30, StaleTs),
		?assert(string:str(Html, "stale") > 0, "must have stale class when data is outdated"),
		?assert(string:str(Html, "Data may be outdated") > 0, "must describe the problem in text (not colour alone)"),
		?assert(string:str(Html, "7h 20m ago") > 0, "must show the age of the oldest source"),
		?assert(string:str(Html, "aria-hidden") > 0, "warning icon must be aria-hidden (text carries the meaning)"),
		?assert(string:str(Html, "data-oldest-source-ts") > 0, "must include data-oldest-source-ts attribute").

	render_freshness_indicator_just_at_threshold_test() ->
		% At exactly the threshold (300s), the indicator should NOT be stale.
		Now = erlang:system_time(second),
		Html = renderFreshnessIndicator(Now - 300, Now - 300),
		?assertEqual(0, string:str(Html, "stale"), "must NOT be stale at exactly the threshold").

	render_freshness_indicator_just_over_threshold_test() ->
		% One second past the threshold should trigger the stale state.
		Now = erlang:system_time(second),
		Html = renderFreshnessIndicator(Now - 30, Now - 301),
		?assert(string:str(Html, "stale") > 0, "must be stale one second past threshold").

	render_dashboard_block_includes_freshness_indicator_test() ->
		% A system with timestamps renders a freshness indicator.
		Now = erlang:system_time(second),
		Systems = [#{
			<<"host">> => <<"example.l42.eu">>,
			<<"name">> => <<"lucos_example">>,
			<<"id">> => <<"lucos_example">>,
			<<"status">> => healthy,
			<<"checks">> => [],
			<<"metrics">> => [],
			<<"last_updated">> => Now - 30,
			<<"oldest_source_ts">> => Now - 30
		}],
		Html = render_dashboard_block(Systems),
		?assert(string:str(Html, "freshness-indicator") > 0, "dashboard must show freshness indicator").

	render_dashboard_block_stale_freshness_test() ->
		% A system with a stale source shows the stale warning.
		Now = erlang:system_time(second),
		Systems = [#{
			<<"host">> => <<"example.l42.eu">>,
			<<"name">> => <<"lucos_example">>,
			<<"id">> => <<"lucos_example">>,
			<<"status">> => healthy,
			<<"checks">> => [],
			<<"metrics">> => [],
			<<"last_updated">> => Now - 30,
			<<"oldest_source_ts">> => Now - 26400
		}],
		Html = render_dashboard_block(Systems),
		?assert(string:str(Html, "stale") > 0, "dashboard must show stale warning for outdated source"),
		?assert(string:str(Html, "Data may be outdated") > 0, "stale warning must contain explanatory text").

	% ── readableName ─────────────────────────────────────────────────────────

	readable_name_test() ->
		?assertEqual("lucos time", readableName("lucos_time")).

	% ── systemDependsOn / classifyFailure / classifyFailures (#296) ───────────

	system_depends_on_no_checks_test() ->
		?assertEqual([], systemDependsOn([])).

	system_depends_on_no_dependson_field_test() ->
		?assertEqual([], systemDependsOn([#{<<"id">> => <<"fetch-info">>, <<"status">> => failing}])).

	% The trap this exists to avoid: the check that actually failed (fetch-info) carries
	% no dependsOn, but a DIFFERENT check on the same system does. systemDependsOn/1 must
	% surface it regardless of which check is currently failing.
	system_depends_on_scans_all_checks_not_just_failing_one_test() ->
		SystemChecks = [
			#{<<"id">> => <<"fetch-info">>, <<"status">> => failing},
			#{<<"id">> => <<"time-api-reachable">>, <<"status">> => unknown, <<"dependsOn">> => [<<"lucos_time">>]}
		],
		?assertEqual(["lucos_time"], systemDependsOn(SystemChecks)).

	system_depends_on_dedupes_and_sorts_test() ->
		SystemChecks = [
			#{<<"id">> => <<"a">>, <<"dependsOn">> => [<<"lucos_b">>, <<"lucos_a">>]},
			#{<<"id">> => <<"b">>, <<"dependsOn">> => [<<"lucos_a">>]}
		],
		?assertEqual(["lucos_a", "lucos_b"], systemDependsOn(SystemChecks)).

	classify_failure_no_dependson_is_root_test() ->
		?assertEqual(root, classifyFailure("lucos_time", [#{<<"id">> => <<"fetch-info">>}], sets:from_list(["lucos_time"], [{version, 2}]))).

	classify_failure_dependency_not_failing_is_root_test() ->
		SystemChecks = [#{<<"id">> => <<"fetch-info">>, <<"dependsOn">> => [<<"lucos_router">>]}],
		FailingIds = sets:from_list(["lucos_weightings"], [{version, 2}]),
		?assertEqual(root, classifyFailure("lucos_weightings", SystemChecks, FailingIds)).

	classify_failure_dependency_failing_is_dependent_test() ->
		SystemChecks = [#{<<"id">> => <<"time-api-reachable">>, <<"dependsOn">> => [<<"lucos_time">>]}],
		FailingIds = sets:from_list(["lucos_weightings", "lucos_time"], [{version, 2}]),
		?assertEqual({dependent, "lucos_time"}, classifyFailure("lucos_weightings", SystemChecks, FailingIds)).

	classify_failure_self_reference_guard_test() ->
		SystemChecks = [#{<<"id">> => <<"self-check">>, <<"dependsOn">> => [<<"lucos_foo">>]}],
		FailingIds = sets:from_list(["lucos_foo"], [{version, 2}]),
		?assertEqual(root, classifyFailure("lucos_foo", SystemChecks, FailingIds)).

	classify_failures_only_includes_failing_systems_test() ->
		Systems = [
			#{<<"name">> => <<"lucos_healthy">>, <<"status">> => healthy, <<"checks">> => []},
			#{<<"name">> => <<"lucos_time">>, <<"status">> => failing, <<"checks">> => []}
		],
		Classifications = classifyFailures(Systems),
		?assertEqual(1, maps:size(Classifications)),
		?assertEqual(root, maps:get("lucos_time", Classifications)).

	classify_failures_incident_shape_test() ->
		% Reproduces the 2026-08-08 lucos_time incident shape: the dependent's failing
		% check (fetch-info) carries no dependsOn on lucos_time; a different check
		% (time-api-reachable, cached from its last successful poll) does.
		Systems = [
			#{<<"name">> => <<"lucos_time">>, <<"status">> => failing, <<"checks">> => [
				#{<<"id">> => <<"fetch-info">>, <<"status">> => failing}
			]},
			#{<<"name">> => <<"lucos_media_weightings">>, <<"status">> => failing, <<"checks">> => [
				#{<<"id">> => <<"fetch-info">>, <<"status">> => failing},
				#{<<"id">> => <<"time-api-reachable">>, <<"status">> => unknown, <<"dependsOn">> => [<<"lucos_time">>]}
			]}
		],
		Classifications = classifyFailures(Systems),
		?assertEqual(root, maps:get("lucos_time", Classifications)),
		?assertEqual({dependent, "lucos_time"}, maps:get("lucos_media_weightings", Classifications)).

	% ── renderFailureRollup ─────────────────────────────────────────────────

	render_failure_rollup_no_dependents_is_empty_test() ->
		?assertEqual("", renderFailureRollup(#{"lucos_a" => root, "lucos_b" => root})).

	render_failure_rollup_empty_map_is_empty_test() ->
		?assertEqual("", renderFailureRollup(#{})).

	render_failure_rollup_single_root_test() ->
		Html = renderFailureRollup(#{
			"lucos_time" => root,
			"lucos_media_weightings" => {dependent, "lucos_time"}
		}),
		?assert(string:str(Html, "2 failing") > 0),
		?assert(string:str(Html, "1 root cause") > 0),
		?assertEqual(0, string:str(Html, "root causes"), "singular root cause must not also match the plural form's substring incorrectly"),
		?assert(string:str(Html, "1 dependent") > 0),
		?assert(string:str(Html, "lucos time") > 0, "root cause name must be human-readable"),
		?assert(string:str(Html, "href=\"#system-lucos_time\"") > 0, "root cause name must link to its system anchor").

	render_failure_rollup_multiple_roots_pluralises_test() ->
		Html = renderFailureRollup(#{
			"lucos_a" => root,
			"lucos_b" => root,
			"lucos_c" => {dependent, "lucos_a"}
		}),
		?assert(string:str(Html, "2 root causes") > 0),
		?assert(string:str(Html, "1 dependent") > 0).

	% Degenerate cycle guard: if every failing system somehow classifies as dependent
	% (no root identified), the rollup must not claim "0 root causes" — it renders nothing.
	render_failure_rollup_no_root_identified_is_empty_test() ->
		?assertEqual("", renderFailureRollup(#{
			"lucos_a" => {dependent, "lucos_b"},
			"lucos_b" => {dependent, "lucos_a"}
		})).

	% ── render_dashboard_block integration (#296) ────────────────────────────

	render_dashboard_block_shows_rollup_and_dependency_note_test() ->
		Systems = [
			#{<<"host">> => <<"time.l42.eu">>, <<"name">> => <<"lucos_time">>, <<"status">> => failing,
			  <<"checks">> => [#{<<"id">> => <<"fetch-info">>, <<"status">> => failing, <<"statusText">> => <<"failing">>}],
			  <<"metrics">> => []},
			#{<<"host">> => <<"weightings.l42.eu">>, <<"name">> => <<"lucos_media_weightings">>, <<"status">> => failing,
			  <<"checks">> => [
				#{<<"id">> => <<"fetch-info">>, <<"status">> => failing, <<"statusText">> => <<"failing">>},
				#{<<"id">> => <<"time-api-reachable">>, <<"status">> => unknown, <<"statusText">> => <<"unknown">>, <<"dependsOn">> => [<<"lucos_time">>]}
			  ],
			  <<"metrics">> => []}
		],
		Html = render_dashboard_block(Systems),
		?assert(string:str(Html, "failure-rollup") > 0, "must render the rollup line"),
		?assert(string:str(Html, "2 failing") > 0),
		?assert(string:str(Html, "1 root cause") > 0),
		?assert(string:str(Html, "dependent-failure") > 0, "dependent system must carry the de-emphasis class"),
		?assert(string:str(Html, "Dependent on") > 0, "dependent system must show an explanatory note"),
		?assert(string:str(Html, "href=\"#system-lucos_time\"") > 0, "dependency note must link back to the root cause"),
		% Never hidden: the dependent system's own failing check must still be fully visible.
		?assert(string:str(Html, "time-api-reachable") > 0).

	% Regression: a dependent must render immediately after its root cause, not float
	% ahead of it by alphabetical accident (lucos_media_weightings < lucos_time by name,
	% but must still appear after it — this is the exact ordering readers hit on #296).
	render_dashboard_block_dependent_clusters_after_root_test() ->
		Systems = [
			#{<<"host">> => <<"time.l42.eu">>, <<"name">> => <<"lucos_time">>, <<"status">> => failing,
			  <<"checks">> => [#{<<"id">> => <<"fetch-info">>, <<"status">> => failing, <<"statusText">> => <<"failing">>}],
			  <<"metrics">> => []},
			#{<<"host">> => <<"weightings.l42.eu">>, <<"name">> => <<"lucos_media_weightings">>, <<"status">> => failing,
			  <<"checks">> => [
				#{<<"id">> => <<"fetch-info">>, <<"status">> => failing, <<"statusText">> => <<"failing">>},
				#{<<"id">> => <<"time-api-reachable">>, <<"status">> => unknown, <<"statusText">> => <<"unknown">>, <<"dependsOn">> => [<<"lucos_time">>]}
			  ],
			  <<"metrics">> => []}
		],
		Html = render_dashboard_block(Systems),
		PosRoot = string:str(Html, "id=\"system-lucos_time\""),
		PosDependent = string:str(Html, "id=\"system-lucos_media_weightings\""),
		?assert(PosRoot > 0, "root system must be rendered"),
		?assert(PosDependent > 0, "dependent system must be rendered"),
		?assert(PosRoot < PosDependent, "root cause must render before its dependent, even though it sorts later alphabetically").

	render_dashboard_block_independent_failures_show_no_rollup_test() ->
		% Two unrelated failing systems (neither depends on the other) — grouping would
		% add no information, so no rollup line and no dependent-failure class.
		Systems = [
			#{<<"host">> => <<"a.l42.eu">>, <<"name">> => <<"lucos_a">>, <<"status">> => failing,
			  <<"checks">> => [#{<<"id">> => <<"fetch-info">>, <<"status">> => failing, <<"statusText">> => <<"failing">>}],
			  <<"metrics">> => []},
			#{<<"host">> => <<"b.l42.eu">>, <<"name">> => <<"lucos_b">>, <<"status">> => failing,
			  <<"checks">> => [#{<<"id">> => <<"fetch-info">>, <<"status">> => failing, <<"statusText">> => <<"failing">>}],
			  <<"metrics">> => []}
		],
		Html = render_dashboard_block(Systems),
		?assertEqual(0, string:str(Html, "failure-rollup")),
		?assertEqual(0, string:str(Html, "dependent-failure")).

-endif.
