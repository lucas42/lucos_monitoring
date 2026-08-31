-module(view_json).
-export([encodeStatus/1, encodeInfo/2]).

encodeStatus(Systems) ->
	EncodedSystems = maps:from_list(lists:map(
		fun (System) ->
			SystemId = maps:get(<<"id">>, System),
			Name = maps:get(<<"name">>, System),
			Status = maps:get(<<"status">>, System),
			% Rebuild checks as a map keyed by check id, omitting the id field from the value
			Checks = maps:from_list([
				{maps:get(<<"id">>, C), maps:without([<<"id">>], C)}
				|| C <- maps:get(<<"checks">>, System, [])]),
			% Rebuild metrics as a map keyed by metric id, omitting the id field from the value
			Metrics = maps:from_list([
				{maps:get(<<"id">>, M), maps:without([<<"id">>], M)}
				|| M <- maps:get(<<"metrics">>, System, [])]),
			SystemJson = #{
				<<"name">>    => Name,
				<<"status">>  => Status,
				<<"checks">>  => Checks,
				<<"metrics">> => Metrics
			},
			{SystemId, SystemJson}
		end, Systems)),
	% Summary counts: healthy, failing, anything else (unknown/buffering/suppressed/pending) → unknown
	{TotalSystems, HealthyCount, FailingCount, UnknownCount} = lists:foldl(
		fun (System, {Total, Healthy, Failing, Unknown}) ->
			case maps:get(<<"status">>, System) of
				healthy -> {Total + 1, Healthy + 1, Failing, Unknown};
				failing -> {Total + 1, Healthy, Failing + 1, Unknown};
				_       -> {Total + 1, Healthy, Failing, Unknown + 1}
			end
		end, {0, 0, 0, 0}, Systems),
	Summary = #{
		<<"total_systems">> => TotalSystems,
		<<"healthy">>       => HealthyCount,
		<<"failing">>       => FailingCount,
		<<"unknown">>       => UnknownCount
	},
	jiffy:encode(#{
		<<"systems">> => EncodedSystems,
		<<"summary">> => Summary
	}).

encodeInfo(Systems, PollStats) ->
	PollMetrics = case maps:get(count, PollStats, 0) > 0 of
		true ->
			#{
				<<"poll-max-duration-ms">> => #{
					<<"value">> => maps:get(max_duration_ms, PollStats, 0),
					<<"techDetail">> => <<"Maximum check duration (ms) across all systems in the most recent poll cycle">>
				},
				<<"poll-mean-duration-ms">> => #{
					<<"value">> => maps:get(mean_duration_ms, PollStats, 0),
					<<"techDetail">> => <<"Mean check duration (ms) across all systems in the most recent poll cycle">>
				},
				<<"poll-failed-checks">> => #{
					<<"value">> => maps:get(failed_count, PollStats, 0),
					<<"techDetail">> => <<"Number of systems whose most recent poll had a fetch-info failure">>
				}
			};
		false ->
			#{}
	end,
	jiffy:encode(#{
		system => <<"lucos_monitoring">>,
		checks => #{},
		metrics => maps:merge(#{
			<<"system-count">> => #{
				<<"value">> => length(Systems),
				<<"techDetail">> => <<"The number of systems being monitored">>
			}
		}, PollMetrics),
		ci => #{
			circle => <<"gh/lucas42/lucos_monitoring">>
		},
		icon => <<"/icon">>,
		network_only => true,
		title => <<"Monitoring">>,
		show_on_homepage => true
	}).

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").

	% encodeInfo: no poll stats (count=0) → only system-count metric
	encodeInfo_no_poll_stats_test() ->
		EmptyStats = #{count => 0, max_duration_ms => 0, mean_duration_ms => 0, failed_count => 0},
		Result = jiffy:decode(encodeInfo([], EmptyStats), [return_maps]),
		Metrics = maps:get(<<"metrics">>, Result),
		?assert(maps:is_key(<<"system-count">>, Metrics)),
		?assertNot(maps:is_key(<<"poll-max-duration-ms">>, Metrics)),
		?assertNot(maps:is_key(<<"poll-mean-duration-ms">>, Metrics)),
		?assertNot(maps:is_key(<<"poll-failed-checks">>, Metrics)).

	% encodeInfo: with poll stats → all four metrics present
	encodeInfo_with_poll_stats_test() ->
		PollStats = #{count => 10, max_duration_ms => 900, mean_duration_ms => 350, failed_count => 2},
		Result = jiffy:decode(encodeInfo([], PollStats), [return_maps]),
		Metrics = maps:get(<<"metrics">>, Result),
		?assert(maps:is_key(<<"system-count">>, Metrics)),
		?assert(maps:is_key(<<"poll-max-duration-ms">>, Metrics)),
		?assert(maps:is_key(<<"poll-mean-duration-ms">>, Metrics)),
		?assert(maps:is_key(<<"poll-failed-checks">>, Metrics)),
		?assertEqual(900, maps:get(<<"value">>, maps:get(<<"poll-max-duration-ms">>, Metrics))),
		?assertEqual(350, maps:get(<<"value">>, maps:get(<<"poll-mean-duration-ms">>, Metrics))),
		?assertEqual(2, maps:get(<<"value">>, maps:get(<<"poll-failed-checks">>, Metrics))).

	% encodeInfo: system-count reflects number of systems
	encodeInfo_system_count_test() ->
		PollStats = #{count => 0},
		Systems = [#{<<"host">> => <<"a">>, <<"name">> => <<"s">>, <<"status">> => healthy, <<"checks">> => [], <<"metrics">> => []},
		           #{<<"host">> => <<"b">>, <<"name">> => <<"t">>, <<"status">> => healthy, <<"checks">> => [], <<"metrics">> => []}],
		Result = jiffy:decode(encodeInfo(Systems, PollStats), [return_maps]),
		?assertEqual(2, maps:get(<<"value">>, maps:get(<<"system-count">>, maps:get(<<"metrics">>, Result)))).

	encodeStatus_empty_test() ->
		Result = jiffy:decode(encodeStatus([]), [return_maps]),
		?assertEqual(#{}, maps:get(<<"systems">>, Result)),
		Summary = maps:get(<<"summary">>, Result),
		?assertEqual(0, maps:get(<<"total_systems">>, Summary)),
		?assertEqual(0, maps:get(<<"healthy">>, Summary)),
		?assertEqual(0, maps:get(<<"failing">>, Summary)),
		?assertEqual(0, maps:get(<<"unknown">>, Summary)).

	encodeStatus_healthy_system_test() ->
		Systems = [#{
			<<"id">>      => <<"lucos_example">>,
			<<"host">>    => <<"example.l42.eu">>,
			<<"name">>    => <<"lucos_example">>,
			<<"status">>  => healthy,
			<<"checks">>  => [#{<<"id">> => <<"fetch-info">>, <<"status">> => healthy, <<"statusText">> => <<"healthy">>, <<"techDetail">> => <<"Fetches /_info">>}],
			<<"metrics">> => []
		}],
		Result = jiffy:decode(encodeStatus(Systems), [return_maps]),
		SystemsMap = maps:get(<<"systems">>, Result),
		System = maps:get(<<"lucos_example">>, SystemsMap),
		?assertEqual(<<"lucos_example">>, maps:get(<<"name">>, System)),
		?assertEqual(<<"healthy">>, maps:get(<<"status">>, System)),
		Summary = maps:get(<<"summary">>, Result),
		?assertEqual(1, maps:get(<<"total_systems">>, Summary)),
		?assertEqual(1, maps:get(<<"healthy">>, Summary)),
		?assertEqual(0, maps:get(<<"failing">>, Summary)),
		?assertEqual(0, maps:get(<<"unknown">>, Summary)).

	encodeStatus_failing_system_test() ->
		Systems = [#{
			<<"id">>      => <<"lucos_broken">>,
			<<"host">>    => <<"broken.l42.eu">>,
			<<"name">>    => <<"lucos_broken">>,
			<<"status">>  => failing,
			<<"checks">>  => [#{<<"id">> => <<"fetch-info">>, <<"status">> => failing, <<"statusText">> => <<"failing">>, <<"techDetail">> => <<"Fetches /_info">>, <<"debug">> => <<"Connection refused">>}],
			<<"metrics">> => []
		}],
		Result = jiffy:decode(encodeStatus(Systems), [return_maps]),
		SystemsMap = maps:get(<<"systems">>, Result),
		System = maps:get(<<"lucos_broken">>, SystemsMap),
		?assertEqual(<<"failing">>, maps:get(<<"status">>, System)),
		Checks = maps:get(<<"checks">>, System),
		FetchInfo = maps:get(<<"fetch-info">>, Checks),
		?assertEqual(<<"failing">>, maps:get(<<"status">>, FetchInfo)),
		?assertEqual(<<"Connection refused">>, maps:get(<<"debug">>, FetchInfo)),
		Summary = maps:get(<<"summary">>, Result),
		?assertEqual(1, maps:get(<<"total_systems">>, Summary)),
		?assertEqual(0, maps:get(<<"healthy">>, Summary)),
		?assertEqual(1, maps:get(<<"failing">>, Summary)),
		?assertEqual(0, maps:get(<<"unknown">>, Summary)).

	encodeStatus_unknown_system_test() ->
		Systems = [#{
			<<"id">>      => <<"lucos_unreachable">>,
			<<"host">>    => <<"unreachable.l42.eu">>,
			<<"name">>    => <<"lucos_unreachable">>,
			<<"status">>  => unknown,
			<<"checks">>  => [#{<<"id">> => <<"fetch-info">>, <<"status">> => unknown, <<"statusText">> => <<"unknown">>}],
			<<"metrics">> => []
		}],
		Result = jiffy:decode(encodeStatus(Systems), [return_maps]),
		SystemsMap = maps:get(<<"systems">>, Result),
		System = maps:get(<<"lucos_unreachable">>, SystemsMap),
		?assertEqual(<<"lucos_unreachable">>, maps:get(<<"name">>, System)),
		?assertEqual(<<"unknown">>, maps:get(<<"status">>, System)),
		Summary = maps:get(<<"summary">>, Result),
		?assertEqual(1, maps:get(<<"total_systems">>, Summary)),
		?assertEqual(0, maps:get(<<"healthy">>, Summary)),
		?assertEqual(0, maps:get(<<"failing">>, Summary)),
		?assertEqual(1, maps:get(<<"unknown">>, Summary)).

	encodeStatus_multiple_systems_summary_test() ->
		Systems = [
			#{<<"id">> => <<"lucos_healthy">>,   <<"host">> => <<"healthy.l42.eu">>,   <<"name">> => <<"lucos_healthy">>,   <<"status">> => healthy,   <<"checks">> => [], <<"metrics">> => []},
			#{<<"id">> => <<"lucos_failing">>,   <<"host">> => <<"failing.l42.eu">>,   <<"name">> => <<"lucos_failing">>,   <<"status">> => failing,   <<"checks">> => [], <<"metrics">> => []},
			#{<<"id">> => <<"lucos_unknown">>,   <<"host">> => <<"unknown.l42.eu">>,   <<"name">> => <<"lucos_unknown">>,   <<"status">> => unknown,   <<"checks">> => [], <<"metrics">> => []},
			#{<<"id">> => <<"lucos_buffering">>, <<"host">> => <<"buffering.l42.eu">>, <<"name">> => <<"lucos_buffering">>, <<"status">> => buffering, <<"checks">> => [], <<"metrics">> => []}
		],
		Result = jiffy:decode(encodeStatus(Systems), [return_maps]),
		Summary = maps:get(<<"summary">>, Result),
		?assertEqual(4, maps:get(<<"total_systems">>, Summary)),
		?assertEqual(1, maps:get(<<"healthy">>, Summary)),
		?assertEqual(1, maps:get(<<"failing">>, Summary)),
		% buffering counts towards unknown in summary (not definitively healthy or failing)
		?assertEqual(2, maps:get(<<"unknown">>, Summary)).

-endif.
