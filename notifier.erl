-module(notifier).
-export([notify/4]).


notify(Host, SystemName, SystemChecks, SystemMetrics) ->
	System = getSystemTitle(Host, SystemName),
	io:format("Send notifications for ~p~n", [System]).


getSystemTitle(Host, Name) ->
	case Name of
		unknown ->
			Host;
		_ ->
			Name
	end.