-module(notifier).
-export([notify/4]).


notify(Host, SystemName, FailingChecks, SystemMetrics) ->
	System = getSystemTitle(Host, SystemName),
	% Use Host for consistent Subject lines so things get bundled into nice threads
	EmailSubject = "Monitoring issue on "++Host,
	EmailBody = getEmailBody(Host, System, FailingChecks, SystemMetrics),
	io:format("Send notifications for ~p~n", [System]),
	sendEmail(EmailSubject, EmailBody).


getSystemTitle(Host, Name) ->
	case Name of
		unknown ->
			Host;
		_ ->
			re:replace(Name, "_", " ", [global, {return,list}])
	end.

getEmailBody(Host, System, FailingChecks, SystemMetrics) ->
	getEmailSummary(System, FailingChecks)
	++ "\r\n\r\n" ++
	getSystemLink(Host)
	++ "\r\n\r\n" ++
	getMetricSummary(SystemMetrics).

getEmailSummary(System, FailingChecks) ->
	FailCountSummary = getFailCountSummary(maps:size(FailingChecks), System),
	maps:fold(fun (CheckId, CheckInfo, Output) ->
		Output ++ "The \""++binary_to_list(CheckId)++"\" check is failing.\r\n"
		++ getSubsection("Tech Details", <<"techDetail">>, CheckInfo)
		++ getSubsection("Debug", <<"debug">>, CheckInfo)
		++ getSubsection("Link", <<"link">>, CheckInfo)
		++ "\r\n"
	end, FailCountSummary ++ "\r\n", FailingChecks).

getSubsection(Label, Key, CheckInfo) ->
	Value = binary_to_list(maps:get(Key, CheckInfo, <<"">>)),
	case Value of
		"" -> "";
		_ -> Label ++ ": " ++ Value ++ "\r\n"
	end.

getFailCountSummary(FailCount, System) ->
	case FailCount of
		0 ->
			"Everything OK on "++System++".";
		1 ->
			"There is 1 failing check on "++System++":";
		_ ->
			"There are "++integer_to_list(FailCount)++" failing checks on "++System++":"
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

sendEmail(Subject, Body) ->
	SendAddress =  os:getenv("SEND_ADDRESS"),
	Password = os:getenv("SEND_PASSWORD"),
	Relay = os:getenv("SMTP_RELAY"),
	To = os:getenv("TO_ADDRESS"),
	Sender = "Lucos Monitoring <"++SendAddress++">",
	CurrentDate = calendar:system_time_to_rfc3339(erlang:system_time(second)),
	Content = "Subject: "++Subject++"\r\nFrom: "++Sender++"\r\nTo: <"++To++">"++"\r\nDate: "++CurrentDate++"\r\n\r\n"++Body,
	Email = {SendAddress, [To], Content},
	Options = [
		{relay, Relay},
		{username, SendAddress},
		{password, Password},
		{tls_options, [
			{verify, verify_peer},
			{cacerts, public_key:cacerts_get()},
			{depth, 10},
			{server_name_indication, Relay}]}
		],
	gen_smtp_client:send(Email, Options, fun(Response) ->
		case Response of
			{ok, _ } ->
				ok;
			{error, send, {permanent_failure, _, ErrorMessage}} ->
				io:format("Error Sending Email: ~p~n", [binary_to_list(ErrorMessage)]);
			{error, send, Error} ->
				io:format("Unhandled Error Sending Email.  ~p~n", [Error]);
			{error, Type, Error} ->
				io:format("Unhandled Email Error of Type ~p.  ~p~n", [Type, Error]);
			_ ->
				io:format("Unexpected Response from sending email. ~p.~n", [Response])
		end
	end).