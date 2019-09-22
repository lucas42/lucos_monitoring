-module(notifier).
-export([notify/4]).


notify(Host, SystemName, SystemChecks, SystemMetrics) ->
	System = getSystemTitle(Host, SystemName),
	% Use Host for consistent Subject lines so things get bundled into nice threads
	EmailSubject = "Monitoring issue on "++Host,
	EmailBody = getEmailBody(Host, System, SystemChecks, SystemMetrics),
	sendEmail(EmailSubject, EmailBody),
	io:format("Send notifications for ~p~n", [System]).


getSystemTitle(Host, Name) ->
	case Name of
		unknown ->
			Host;
		_ ->
			re:replace(Name, "_", " ", [global, {return,list}])
	end.

getEmailBody(Host, System, SystemChecks, SystemMetrics) ->
	getEmailSummary(System, SystemChecks)
	++ "\r\n\r\n" ++
	getSystemLink(Host)
	++ "\r\n\r\n" ++
	getMetricSummary(SystemMetrics).

getEmailSummary(System, SystemChecks) ->
	FailingChecks = maps:filter(fun(_,C) -> isCheckFailing(C) end, SystemChecks),
	FailingCheckNames = maps:keys(FailingChecks),
	FailCount = maps:size(FailingChecks),
	case FailCount of
		0 ->
			"Everything OK on "++System;
		1 ->
			FailingCheckName = binary_to_list(lists:last(FailingCheckNames)),
			"The "++FailingCheckName++" check is failing on "++System;
		_ ->
			"There are "++integer_to_list(FailCount)++" failing checks on "++System
	end.

getSystemLink(Host) ->
	"https://monitoring.l42.eu/#host-"++Host.

getMetricSummary(SystemMetrics) ->
	case maps:size(SystemMetrics) of
		0 ->
			"";
		_ ->
			io_lib:format("** System Metrics **~n~p~n",[SystemMetrics])
	end.

isCheckFailing(CheckInfo) ->
	not maps:get(<<"ok">>, CheckInfo, false).

sendEmail(Subject, Body) ->
	SendAddress =  os:getenv("SEND_ADDRESS"),
	Password = os:getenv("SEND_PASSWORD"),
	Relay = os:getenv("SMTP_RELAY"),
	To = os:getenv("TO_ADDRESS"),
	Sender = "Lucos Monitoring <"++SendAddress++">",
	Content = "Subject: "++Subject++"\r\nFrom: "++Sender++"\r\nTo: <"++To++">\r\n\r\n"++Body,
	Email = {SendAddress, [To], Content},
	Options = [{relay, Relay}, {username, SendAddress}, {password, Password}],
	try gen_smtp_client:send_blocking(Email, Options) of
		_ -> ok
	catch
		ExceptionClass:Term:StackTrace ->
			case Term of
				{permanent_failure, ErrorMessage} ->
					io:format("Error Sending Email: ~p~n", [binary_to_list(ErrorMessage)]);
				_ ->
					io:format("Unhandled Error Sending Email.  ExceptionClass: ~p Term: ~p StackTrace: ~p~n", [ExceptionClass, Term, StackTrace])
			end,
			fail
	end.