-module(check_normalisation).
-export([mergeSourceChecks/1, normaliseChecks/3, meaningfulChange/2, failingChecks/1]).

% Alert-suppression chain: UnknownsGate → FailsGate.
%
% UnknownsGate (replaceUnknowns/4): tracks consecutiveUnknownsCount.  When a check
%   reports ok: unknown for ≥?CONSECUTIVE_UNKNOWNS_THRESHOLD consecutive polls it
%   flips to ok: false so the FailsGate can evaluate it.  Used by fetchers that probe
%   a third-party service (e.g. CircleCI) — third-party unreachability says nothing
%   about the monitored system's health, so "I couldn't tell" is the right signal.
%
% FailsGate (applyFailThreshold/2): tracks consecutiveFailsCount.  When ok: false
%   is seen for ≥failThreshold consecutive polls (default 1) the failure is surfaced.
%   Used by fetchers that directly probe the system itself (fetch-info, tls-certificate)
%   which stamp failThreshold: 2 to absorb single-poll transient blips.
%
% The two gates chain: an unknown that persists long enough becomes a false (UnknownsGate),
% which then goes through the FailsGate with the default threshold of 1.  A direct false
% (e.g. a workflow failure from circleci) skips the UnknownsGate entirely and is only
% subject to the FailsGate.
-define(CONSECUTIVE_UNKNOWNS_THRESHOLD, 5).

% Merges checks from all sources into a single flat map.
% The current sources (info, circleci) write disjoint check key sets,
% so fold ordering doesn't matter in practice. If a future source shares
% keys with an existing one, override order will be non-deterministic
% (map fold order over atom keys is unspecified in Erlang) — revisit then.
mergeSourceChecks(SourceChecksMap) ->
	maps:fold(fun(_, Checks, Acc) ->
		maps:merge(Acc, Checks)
	end, #{}, SourceChecksMap).

% If there's a problem with the 'fetch-info' check, merge the old and new checks to avoid flapiness
mergeMissingInfoChecks(OldChecks, NewChecks) ->
	case maps:get(<<"ok">>, maps:get(<<"fetch-info">>, NewChecks, #{<<"ok">> => unknown}), unknown) of
		true ->
			NewChecks;
		_ ->
			maps:merge(OldChecks, NewChecks)
	end.

% ── UnknownsGate ──────────────────────────────────────────────────────────────
% Replaces any ok: unknown with the previously-known value until
% ?CONSECUTIVE_UNKNOWNS_THRESHOLD consecutive unknowns are seen, at which point ok
% is flipped to false so the FailsGate (below) can evaluate it.
%
% CountableKeys is the set of check keys reported in the current source update.
% Only those keys increment their consecutiveUnknownsCount — this prevents
% double-counting when checks from one source are carried forward in the merged
% view during another source's update.
replaceUnknowns(OldChecks, NewChecks, Iterator, CountableKeys) ->
	case maps:next(Iterator) of
		{Key, NewCheck, NextIterator} ->
			NormalisedCheck = case maps:get(<<"ok">>, NewCheck, unknown) of
				unknown ->
					OldCheck = maps:get(Key, OldChecks, #{<<"ok">> => unknown}),
					OldCount = maps:get(<<"consecutiveUnknownsCount">>, OldCheck, 0),
					NewCount = case sets:is_element(Key, CountableKeys) of
						true -> OldCount + 1;  % This check was in the current source update
						false -> OldCount  % Carried over from a previous source, don't re-increment
					end,
					IncrementedCheck = maps:put(<<"consecutiveUnknownsCount">>, NewCount, NewCheck),
					case NewCount >= ?CONSECUTIVE_UNKNOWNS_THRESHOLD of
						true ->
							maps:put(<<"ok">>, false, IncrementedCheck);
						false ->
							logger:notice("Not sending alert for ~p as there has only been ~p recurring failures so far.", [Key, NewCount]),
							maps:put(<<"ok">>, maps:get(<<"ok">>, OldCheck, unknown), IncrementedCheck)
					end;
				_ ->
					maps:put(<<"consecutiveUnknownsCount">>, 0, NewCheck)
			end,
			maps:put(Key, NormalisedCheck, replaceUnknowns(OldChecks, NewChecks, NextIterator, CountableKeys));
		none ->
			maps:new()
	end.

% ── FailsGate ─────────────────────────────────────────────────────────────────
% Holds the previous ok state until N consecutive ok: false values are seen.
% failThreshold defaults to 1 (alert on first failure); direct-probe fetchers
% (fetch-info, tls-certificate) stamp failThreshold: 2 to absorb single-poll
% transient blips during deploys or container restarts.
% consecutiveFailsCount resets to 0 on recovery.
applyFailThreshold(OldChecks, NewChecks) ->
	maps:map(fun(Key, NewCheck) ->
		case maps:get(<<"ok">>, NewCheck, unknown) of
			false ->
				FailThreshold = maps:get(<<"failThreshold">>, NewCheck, 1),
				OldCheck = maps:get(Key, OldChecks, #{}),
				OldFailCount = maps:get(<<"consecutiveFailsCount">>, OldCheck, 0),
				NewFailCount = OldFailCount + 1,
				CheckWithCount = maps:put(<<"consecutiveFailsCount">>, NewFailCount, NewCheck),
				case NewFailCount < FailThreshold of
					true ->
						% Not yet at threshold — hold the previous ok value
						OldOk = maps:get(<<"ok">>, OldCheck, unknown),
						maps:put(<<"ok">>, OldOk, CheckWithCount);
					false ->
						CheckWithCount
				end;
			_ ->
				% Healthy or unknown — reset the failure counter
				maps:put(<<"consecutiveFailsCount">>, 0, NewCheck)
		end
	end, NewChecks).

% Reduces monitoring flapiness by running both alert-suppression gates in sequence.
% CountableKeys is the set of check keys from the current source update that are
% reporting unknown — only those may increment consecutiveUnknownsCount, preventing
% double-counting when checks from one source are carried forward during another
% source's update.
normaliseChecks(OldChecks, NewChecks, CountableKeys) ->
	MergedChecks = mergeMissingInfoChecks(OldChecks, NewChecks),
	AfterUnknowns = replaceUnknowns(OldChecks, MergedChecks, maps:iterator(MergedChecks, reversed), CountableKeys),
	applyFailThreshold(OldChecks, AfterUnknowns).

% Decides whether the checks have changed in a meaningful way (ie ignore "unknown" states)
meaningfulChange(OldChecks, NewChecks) ->
	NewFailingChecks = failingChecks(NewChecks),
	OldFailingChecks = failingChecks(OldChecks),
	maps:keys(OldFailingChecks) /= maps:keys(NewFailingChecks).

failingChecks(Checks) ->
	maps:filter(fun(_, Check) ->
		maps:get(<<"ok">>, Check, unknown) == false
	end, Checks).

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").

	nomaliseChecks_test() ->
		CiCountable = sets:from_list([<<"ci">>], [{version, 2}]),
		FetchInfoCountable = sets:from_list([<<"fetch-info">>], [{version, 2}]),
		% Single unknown check: ci goes from ok to unknown (count 0→1, ok held as true)
		?assertEqual(#{<<"ci">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 1, <<"consecutiveFailsCount">> => 0}, <<"fetch-info">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 0}}, normaliseChecks(#{<<"ci">> => #{<<"ok">> => true}}, #{<<"ci">> => #{<<"ok">> => unknown}, <<"fetch-info">> => #{<<"ok">> => true}}, CiCountable)),
		% Second consecutive unknown for ci (count 1→2, ok still held as true)
		?assertEqual(#{<<"ci">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 2, <<"consecutiveFailsCount">> => 0}, <<"fetch-info">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 0}}, normaliseChecks(#{<<"ci">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 1}, <<"fetch-info">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 2}}, #{<<"ci">> => #{<<"ok">> => unknown}, <<"fetch-info">> => #{<<"ok">> => true}}, CiCountable)),
		% Third consecutive unknown for ci (count 2→3, ok still held as true — below the threshold of 5)
		?assertEqual(#{<<"ci">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 3, <<"consecutiveFailsCount">> => 0}, <<"fetch-info">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 0}}, normaliseChecks(#{<<"ci">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 2}}, #{<<"ci">> => #{<<"ok">> => unknown}, <<"fetch-info">> => #{<<"ok">> => true}}, CiCountable)),
		% Fourth consecutive unknown for ci (count 3→4, ok still held as true — one below the threshold)
		?assertEqual(#{<<"ci">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 4, <<"consecutiveFailsCount">> => 0}, <<"fetch-info">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 0}}, normaliseChecks(#{<<"ci">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 3}}, #{<<"ci">> => #{<<"ok">> => unknown}, <<"fetch-info">> => #{<<"ok">> => true}}, CiCountable)),
		% Fifth consecutive unknown for ci (count 4→5, hits ?CONSECUTIVE_UNKNOWNS_THRESHOLD, ok flips to false and alerts)
		?assertEqual(#{<<"ci">> => #{<<"ok">> => false, <<"consecutiveUnknownsCount">> => 5, <<"consecutiveFailsCount">> => 1}, <<"fetch-info">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 0}}, normaliseChecks(#{<<"ci">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 4}}, #{<<"ci">> => #{<<"ok">> => unknown}, <<"fetch-info">> => #{<<"ok">> => true}}, CiCountable)),
		% fetch-info goes unknown while other checks are carried forward from old state
		?assertEqual(#{<<"item-count">> => #{<<"ok">> => false, <<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 1}, <<"api-check">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 0}, <<"fetch-info">> => #{<<"ok">> => true,  <<"consecutiveUnknownsCount">> => 1, <<"consecutiveFailsCount">> => 0}}, normaliseChecks(#{<<"item-count">> => #{<<"ok">> => false}, <<"api-check">> => #{<<"ok">> => true}, <<"fetch-info">> => #{<<"ok">> => true}}, #{<<"fetch-info">> => #{<<"ok">> => unknown}}, FetchInfoCountable)).

	meaningfulChange_test() ->
		?assertEqual(false, meaningfulChange(#{<<"ci">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 1}, <<"fetch-info">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0}}, #{<<"ci">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0}, <<"fetch-info">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0}})),
		?assertEqual(true, meaningfulChange(#{<<"ci">> => #{<<"ok">> => false, <<"consecutiveUnknownsCount">> => 3}, <<"fetch-info">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0}}, #{<<"ci">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 2}, <<"fetch-info">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0}})),
		?assertEqual(true, meaningfulChange(#{<<"ci">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 1}, <<"fetch-info">> => #{<<"ok">> => false, <<"consecutiveUnknownsCount">> => 0}}, #{<<"ci">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0}, <<"fetch-info">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0}})),
		?assertEqual(true, meaningfulChange(#{<<"ci">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0}, <<"fetch-info">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0}}, #{<<"ci">> => #{<<"ok">> => false, <<"consecutiveUnknownsCount">> => 0}, <<"fetch-info">> => #{<<"ok">> => false, <<"consecutiveUnknownsCount">> => 4}})).

	mergeSourceChecks_test() ->
		% Two disjoint sources produce a flat merged map
		SourceChecksMap = #{
			info => #{<<"fetch-info">> => #{<<"ok">> => true}, <<"tls-certificate">> => #{<<"ok">> => true}},
			circleci => #{<<"circleci">> => #{<<"ok">> => false}}
		},
		Result = mergeSourceChecks(SourceChecksMap),
		?assertEqual(#{
			<<"fetch-info">> => #{<<"ok">> => true},
			<<"tls-certificate">> => #{<<"ok">> => true},
			<<"circleci">> => #{<<"ok">> => false}
		}, Result).

	mergeSourceChecks_empty_test() ->
		?assertEqual(#{}, mergeSourceChecks(#{})).

	% With default failThreshold (1), a single failure is reported immediately.
	failThreshold_default_alerts_immediately_test() ->
		OldChecks = #{<<"db-check">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 0}},
		NewChecks = #{<<"db-check">> => #{<<"ok">> => false}},
		% db-check is false (not unknown), so CountableKeys is empty
		Result = normaliseChecks(OldChecks, NewChecks, sets:new([{version, 2}])),
		?assertEqual(false, maps:get(<<"ok">>, maps:get(<<"db-check">>, Result))),
		?assertEqual(1, maps:get(<<"consecutiveFailsCount">>, maps:get(<<"db-check">>, Result))).

	% With failThreshold 3, failures 1 and 2 hold the previous ok state.
	% On the third consecutive failure, ok flips to false.
	failThreshold_holds_until_threshold_test() ->
		NewChecks = #{<<"db-check">> => #{<<"ok">> => false, <<"failThreshold">> => 3}},
		% db-check is false (not unknown), so CountableKeys is empty
		EmptyCountable = sets:new([{version, 2}]),
		% First failure — consecutiveFailsCount goes to 1, ok stays true (held from old)
		OldChecks1 = #{<<"db-check">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 0}},
		Result1 = normaliseChecks(OldChecks1, NewChecks, EmptyCountable),
		?assertEqual(true, maps:get(<<"ok">>, maps:get(<<"db-check">>, Result1))),
		?assertEqual(1, maps:get(<<"consecutiveFailsCount">>, maps:get(<<"db-check">>, Result1))),
		% Second failure — consecutiveFailsCount goes to 2, still held
		Result2 = normaliseChecks(Result1, NewChecks, EmptyCountable),
		?assertEqual(true, maps:get(<<"ok">>, maps:get(<<"db-check">>, Result2))),
		?assertEqual(2, maps:get(<<"consecutiveFailsCount">>, maps:get(<<"db-check">>, Result2))),
		% Third failure — consecutiveFailsCount goes to 3, now ok flips to false
		Result3 = normaliseChecks(Result2, NewChecks, EmptyCountable),
		?assertEqual(false, maps:get(<<"ok">>, maps:get(<<"db-check">>, Result3))),
		?assertEqual(3, maps:get(<<"consecutiveFailsCount">>, maps:get(<<"db-check">>, Result3))).

	% Recovery (ok: true) resets consecutiveFailsCount to 0.
	failThreshold_recovery_resets_count_test() ->
		OldChecks = #{<<"db-check">> => #{<<"ok">> => true, <<"consecutiveUnknownsCount">> => 0, <<"consecutiveFailsCount">> => 2, <<"failThreshold">> => 3}},
		NewChecks = #{<<"db-check">> => #{<<"ok">> => true, <<"failThreshold">> => 3}},
		% db-check is true (not unknown), so CountableKeys is empty
		Result = normaliseChecks(OldChecks, NewChecks, sets:new([{version, 2}])),
		?assertEqual(true, maps:get(<<"ok">>, maps:get(<<"db-check">>, Result))),
		?assertEqual(0, maps:get(<<"consecutiveFailsCount">>, maps:get(<<"db-check">>, Result))).
-endif.
