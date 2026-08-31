-module(depends_on).
-export([is_dependency_suppressed/3, find_dependent_systems/2]).
-include("monitoring_state.hrl").

% Normalises a check's dependsOn field into a list of system ID strings.
% Accepts either:
%   - absent: []
%   - single binary (legacy single-dep shape): [binary_to_list(B)]
%   - list of binaries (aggregate/cross-cutting shape): [binary_to_list(E) || ...]
% Any unrecognised value (or non-binary element inside a list) is silently
% dropped. See ADR-0002.
normalise_depends_on(Check) ->
	case maps:get(<<"dependsOn">>, Check, undefined) of
		undefined -> [];
		B when is_binary(B) -> [binary_to_list(B)];
		L when is_list(L) ->
			[binary_to_list(E) || E <- L, is_binary(E)];
		_ -> []
	end.

% Returns true if any system in this check's normalised dependsOn list is
% currently under an active suppression window. OR semantics: a single
% suppressed element is sufficient. Guards against self-references
% (CurrentSystem appearing in the list) and treats pending_verification as
% "suppression lifted" per element. Does NOT follow transitive dependsOn
% chains — list elements are not themselves resolved against their own
% dependsOn declarations. See ADR-0002.
is_dependency_suppressed(Check, CurrentSystem, SuppressionMap) ->
	DependsOnList = normalise_depends_on(Check),
	lists:any(fun(DependsOnStr) ->
		% Guard: ignore self-references to prevent circular evaluation
		case DependsOnStr =:= CurrentSystem of
			true -> false;
			false ->
				case maps:get(DependsOnStr, SuppressionMap, undefined) of
					undefined -> false;
					#pending_verification{} -> false;  % Suppression has been lifted
					#suppression_window{expiry_time = ExpiryTime} ->
						Now = erlang:system_time(second),
						Now < ExpiryTime
				end
		end
	end, DependsOnList).

% Returns a list of system IDs whose normalised checks include TargetSystem
% in their dependsOn list (after polymorphic normalisation). Used to cascade
% pending_verification when TargetSystem unsuppresses. Excludes TargetSystem
% itself (self-reference guard).
find_dependent_systems(TargetSystem, SystemMap) ->
	lists:usort(maps:fold(fun(System, #system_state{source_checks_map = SourceChecksMap}, Acc) ->
		case System =:= TargetSystem of
			true -> Acc;  % Guard: skip self
			false ->
				MergedChecks = check_normalisation:mergeSourceChecks(SourceChecksMap),
				HasDependency = maps:fold(fun(_, Check, Found) ->
					Found orelse lists:member(TargetSystem, normalise_depends_on(Check))
				end, false, MergedChecks),
				case HasDependency of
					true -> [System | Acc];
					false -> Acc
				end
		end
	end, [], SystemMap)).

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").

	% is_dependency_suppressed: check with no dependsOn field → false
	is_dependency_suppressed_no_field_test() ->
		?assertEqual(false, is_dependency_suppressed(#{<<"ok">> => false}, "lucos_foo", #{})).

	% is_dependency_suppressed: dependsOn system is not in SuppressionMap → false
	is_dependency_suppressed_system_not_suppressed_test() ->
		Check = #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>},
		?assertEqual(false, is_dependency_suppressed(Check, "lucos_time", #{})).

	% is_dependency_suppressed: dependsOn system has an active suppression window → true
	is_dependency_suppressed_active_suppression_test() ->
		Check = #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>},
		FutureExpiry = erlang:system_time(second) + 600,
		SuppressionMap = #{"lucos_eolas" => #suppression_window{expiry_time = FutureExpiry, pre_existing = #{}}},
		?assertEqual(true, is_dependency_suppressed(Check, "lucos_time", SuppressionMap)).

	% is_dependency_suppressed: dependsOn system is in pending_verification (suppression lifted) → false
	is_dependency_suppressed_pending_verification_test() ->
		Check = #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>},
		PendingSources = sets:from_list([info], [{version, 2}]),
		SuppressionMap = #{"lucos_eolas" => #pending_verification{sources = PendingSources}},
		?assertEqual(false, is_dependency_suppressed(Check, "lucos_time", SuppressionMap)).

	% is_dependency_suppressed: self-reference guard — dependsOn points to the same system → false
	is_dependency_suppressed_self_reference_test() ->
		Check = #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_foo">>},
		FutureExpiry = erlang:system_time(second) + 600,
		SuppressionMap = #{"lucos_foo" => #suppression_window{expiry_time = FutureExpiry, pre_existing = #{}}},
		?assertEqual(false, is_dependency_suppressed(Check, "lucos_foo", SuppressionMap)).

	% is_dependency_suppressed: suppression window has expired → false
	is_dependency_suppressed_expired_test() ->
		Check = #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>},
		PastExpiry = erlang:system_time(second) - 1,
		SuppressionMap = #{"lucos_eolas" => #suppression_window{expiry_time = PastExpiry, pre_existing = #{}}},
		?assertEqual(false, is_dependency_suppressed(Check, "lucos_time", SuppressionMap)).

	% find_dependent_systems: returns system IDs with checks declaring dependsOn TargetSystem
	find_dependent_systems_basic_test() ->
		SystemMap = #{
			"lucos_time" => #system_state{host="host1.example.com", system_type=system, source_checks_map=#{info => #{
				<<"eolas">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>}
			}}},
			"lucos_arachne" => #system_state{host="host2.example.com", system_type=system, source_checks_map=#{info => #{
				<<"triplestore">> => #{<<"ok">> => true}
			}}}
		},
		?assertEqual(["lucos_time"], find_dependent_systems("lucos_eolas", SystemMap)).

	% find_dependent_systems: no systems depend on target → empty list
	find_dependent_systems_none_test() ->
		SystemMap = #{
			"lucos_time" => #system_state{host="host1.example.com", system_type=system, source_checks_map=#{info => #{
				<<"eolas">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>}
			}}}
		},
		?assertEqual([], find_dependent_systems("some.other.system", SystemMap)).

	% find_dependent_systems: self-reference is excluded
	find_dependent_systems_excludes_self_test() ->
		SystemMap = #{
			"lucos_eolas" => #system_state{host="host1.example.com", system_type=system, source_checks_map=#{info => #{
				<<"db">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>}
			}}}
		},
		?assertEqual([], find_dependent_systems("lucos_eolas", SystemMap)).

	% find_dependent_systems: multiple systems can depend on the same target
	find_dependent_systems_multiple_test() ->
		SystemMap = #{
			"lucos_time" => #system_state{host="host1.example.com", system_type=system, source_checks_map=#{info => #{
				<<"eolas">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>}
			}}},
			"lucos_arachne" => #system_state{host="host2.example.com", system_type=system, source_checks_map=#{info => #{
				<<"eolas-data">> => #{<<"ok">> => false, <<"dependsOn">> => <<"lucos_eolas">>}
			}}}
		},
		Deps = find_dependent_systems("lucos_eolas", SystemMap),
		?assertEqual(["lucos_arachne", "lucos_time"], lists:sort(Deps)).

	% --- Polymorphic dependsOn (ADR-0002): list-shape tests ---
	% is_dependency_suppressed: list shape, none of the listed systems suppressed → false
	is_dependency_suppressed_list_none_suppressed_test() ->
		Check = #{<<"ok">> => false, <<"dependsOn">> => [<<"lucos_a">>, <<"lucos_b">>]},
		?assertEqual(false, is_dependency_suppressed(Check, "lucos_loganne", #{})).

	% is_dependency_suppressed: list shape, one listed system actively suppressed → true (OR semantics)
	is_dependency_suppressed_list_one_suppressed_test() ->
		Check = #{<<"ok">> => false, <<"dependsOn">> => [<<"lucos_a">>, <<"lucos_b">>]},
		FutureExpiry = erlang:system_time(second) + 600,
		SuppressionMap = #{"lucos_b" => #suppression_window{expiry_time = FutureExpiry, pre_existing = #{}}},
		?assertEqual(true, is_dependency_suppressed(Check, "lucos_loganne", SuppressionMap)).

	% is_dependency_suppressed: list shape, one listed system in pending_verification → false (suppression lifted for that element)
	is_dependency_suppressed_list_pending_verification_test() ->
		Check = #{<<"ok">> => false, <<"dependsOn">> => [<<"lucos_a">>, <<"lucos_b">>]},
		PendingSources = sets:from_list([info]),
		SuppressionMap = #{"lucos_b" => #pending_verification{sources = PendingSources}},
		?assertEqual(false, is_dependency_suppressed(Check, "lucos_loganne", SuppressionMap)).

	% is_dependency_suppressed: list shape containing the current system (self-reference) →
	% the self element is ignored, but other elements still get evaluated normally
	is_dependency_suppressed_list_self_reference_test() ->
		Check = #{<<"ok">> => false, <<"dependsOn">> => [<<"lucos_loganne">>, <<"lucos_b">>]},
		FutureExpiry = erlang:system_time(second) + 600,
		% Self is "suppressed" too, but that element is ignored; lucos_b is not suppressed → false
		SuppressionMap = #{"lucos_loganne" => #suppression_window{expiry_time = FutureExpiry, pre_existing = #{}}},
		?assertEqual(false, is_dependency_suppressed(Check, "lucos_loganne", SuppressionMap)),
		% Now suppress lucos_b — self is still ignored, but the other element triggers true
		SuppressionMap2 = #{
			"lucos_loganne" => #suppression_window{expiry_time = FutureExpiry, pre_existing = #{}},
			"lucos_b" => #suppression_window{expiry_time = FutureExpiry, pre_existing = #{}}
		},
		?assertEqual(true, is_dependency_suppressed(Check, "lucos_loganne", SuppressionMap2)).

	% is_dependency_suppressed: list shape, one element expired and one element active → true
	% (the active one wins under OR semantics)
	is_dependency_suppressed_list_mixed_expiry_test() ->
		Check = #{<<"ok">> => false, <<"dependsOn">> => [<<"lucos_a">>, <<"lucos_b">>]},
		PastExpiry = erlang:system_time(second) - 1,
		FutureExpiry = erlang:system_time(second) + 600,
		SuppressionMap = #{
			"lucos_a" => #suppression_window{expiry_time = PastExpiry, pre_existing = #{}},
			"lucos_b" => #suppression_window{expiry_time = FutureExpiry, pre_existing = #{}}
		},
		?assertEqual(true, is_dependency_suppressed(Check, "lucos_loganne", SuppressionMap)).

	% is_dependency_suppressed: empty list shape → false (nothing to suppress against)
	is_dependency_suppressed_list_empty_test() ->
		Check = #{<<"ok">> => false, <<"dependsOn">> => []},
		FutureExpiry = erlang:system_time(second) + 600,
		SuppressionMap = #{"lucos_a" => #suppression_window{expiry_time = FutureExpiry, pre_existing = #{}}},
		?assertEqual(false, is_dependency_suppressed(Check, "lucos_loganne", SuppressionMap)).

	% find_dependent_systems: target appears as one of several elements in a list-shaped dependsOn
	find_dependent_systems_list_multi_element_test() ->
		SystemMap = #{
			"lucos_loganne" => #system_state{host="host1.example.com", system_type=system, source_checks_map=#{info => #{
				<<"webhook-error-rate">> =>
					#{<<"ok">> => false, <<"dependsOn">> => [<<"lucos_a">>, <<"lucos_eolas">>, <<"lucos_b">>]}
			}}}
		},
		?assertEqual(["lucos_loganne"], find_dependent_systems("lucos_eolas", SystemMap)).

	% find_dependent_systems: target is the only element in a singleton list
	find_dependent_systems_list_singleton_test() ->
		SystemMap = #{
			"lucos_loganne" => #system_state{host="host1.example.com", system_type=system, source_checks_map=#{info => #{
				<<"webhook-error-rate">> =>
					#{<<"ok">> => false, <<"dependsOn">> => [<<"lucos_eolas">>]}
			}}}
		},
		?assertEqual(["lucos_loganne"], find_dependent_systems("lucos_eolas", SystemMap)).

	% find_dependent_systems: self-reference inside a list is excluded
	find_dependent_systems_list_self_reference_test() ->
		SystemMap = #{
			"lucos_loganne" => #system_state{host="host1.example.com", system_type=system, source_checks_map=#{info => #{
				<<"webhook-error-rate">> =>
					#{<<"ok">> => false, <<"dependsOn">> => [<<"lucos_loganne">>, <<"lucos_a">>]}
			}}}
		},
		?assertEqual([], find_dependent_systems("lucos_loganne", SystemMap)).

	% Router's own checks carry dependsOn: [lucos_router, lucos_dns] (via make_direct_probe_check/1).
	% During lucos_router's own deploy window, is_dependency_suppressed must return false for
	% router's own checks: the lucos_router element in the list is skipped by the self-reference
	% guard, and lucos_dns is not suppressed. This confirms the guard holds with list-valued deps.
	% (Router's checks are already suppressed by its own deploy-window entry; this test confirms
	% the self-reference guard doesn't produce an unrelated false-positive suppression path.)
	router_own_checks_not_self_suppressed_by_list_deps_test() ->
		RouterCheck = #{<<"ok">> => false, <<"dependsOn">> => [<<"lucos_router">>, <<"lucos_dns">>]},
		FutureExpiry = erlang:system_time(second) + 600,
		% Only lucos_router has an active suppression window; lucos_dns does not.
		SuppressionMap = #{"lucos_router" => #suppression_window{expiry_time = FutureExpiry, pre_existing = #{}}},
		% Evaluating from lucos_router's own perspective — self-reference guard must skip
		% lucos_router from the list, and lucos_dns is not in SuppressionMap → false.
		?assertEqual(false, is_dependency_suppressed(RouterCheck, "lucos_router", SuppressionMap)).
-endif.
