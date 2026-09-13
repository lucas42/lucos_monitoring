-module(email).
-export([notify/1]).


% Sends a monitoring email for a system. Accepts a Notification map.
% Suppressed alerts (deploy windows / dependency suppression) are silently dropped —
% the first clause below does that. Every other notification that reaches this module
% is sent, including a recovery (empty failing_checks → "Everything OK on X."): the
% caller in monitoring_state_server only emits a recovery once it has confirmed a
% non-suppressed alert email already went out for the episode, so any recovery arriving
% here is a genuine all-clear that closes a real alert thread (see ADR-0003 / #264).
% Other keys in the map are accepted but unused.
notify(#{suppressed := true}) ->
	% Do not send emails during a suppressed deploy window or for dependency-suppressed checks
	ok;
notify(#{host := Host, system := SystemName, failing_checks := FailingChecks, metrics := SystemMetrics}) ->
	System = getSystemTitle(Host, SystemName),
	% Use Host for consistent Subject lines so things get bundled into nice threads;
	% fall back to system name when there's no host (e.g. components without a domain).
	SubjectRef = case Host of "" -> SystemName; _ -> Host end,
	EmailSubject = "Monitoring issue on "++SubjectRef,
	EmailBody = getEmailBody(SystemName, System, FailingChecks, SystemMetrics),
	logger:notice("Send notifications for ~p", [System]),
	sendEmail(EmailSubject, EmailBody).


getSystemTitle(Host, Name) ->
	case Name of
		unknown ->
			Host;
		_ ->
			re:replace(Name, "_", " ", [global, {return,list}])
	end.

getEmailBody(SystemId, System, FailingChecks, SystemMetrics) ->
	getEmailSummary(System, FailingChecks)
	++ "\r\n\r\n" ++
	getSystemLink(SystemId)
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

getSystemLink(SystemId) ->
	AppOrigin = os:getenv("APP_ORIGIN", ""),
	AppOrigin ++ "/#system-" ++ SystemId.

getMetricSummary(SystemMetrics) ->
	case maps:size(SystemMetrics) of
		0 ->
			"";
		_ ->
			"** System Metrics **\r\n" ++
			maps:fold(fun (MetricId, Metric, Acc) ->
				Id = binary_to_list(MetricId),
				Value = maps:get(<<"value">>, Metric, 0),
				ValueStr = lists:flatten(io_lib:format("~p", [Value])),
				TechDetail = binary_to_list(maps:get(<<"techDetail">>, Metric, <<"">>)),
				TechDetailStr = case TechDetail of
					"" -> "";
					_ -> " (" ++ TechDetail ++ ")"
				end,
				Acc ++ Id ++ ": " ++ ValueStr ++ TechDetailStr ++ "\r\n"
			end, "", SystemMetrics)
	end.

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").

	getMetricSummary_empty_test() ->
		?assertEqual("", getMetricSummary(#{})).

	getMetricSummary_single_metric_test() ->
		Metrics = #{<<"request-count">> => #{<<"value">> => 99, <<"techDetail">> => <<"Total HTTP requests">>}},
		Result = getMetricSummary(Metrics),
		?assert(string:str(Result, "** System Metrics **") > 0),
		?assert(string:str(Result, "request-count") > 0),
		?assert(string:str(Result, "99") > 0),
		?assert(string:str(Result, "Total HTTP requests") > 0).

	% Regression: non-ASCII bytes (em-dash U+2014, UTF-8 bytes 226,128,148) must NOT
	% appear as Erlang integer-list form or binary term form in the email body.
	getMetricSummary_nonascii_techdetail_test() ->
		Metrics = #{<<"last-update">> => #{<<"value">> => 1234567890, <<"techDetail">> => <<"Seconds \xe2\x80\x94 last call">>}},
		Result = lists:flatten(getMetricSummary(Metrics)),
		?assertEqual(0, string:str(Result, "226,128,148"), "techDetail must not appear as integer-list form"),
		?assertEqual(0, string:str(Result, "<<226"), "techDetail must not appear as binary term form"),
		?assert(string:str(Result, "last-update") > 0, "metric ID must appear in email").

	getMetricSummary_no_techdetail_test() ->
		Metrics = #{<<"cpu-usage">> => #{<<"value">> => 42}},
		Result = getMetricSummary(Metrics),
		?assert(string:str(Result, "cpu-usage") > 0),
		?assert(string:str(Result, "42") > 0),
		% No parenthetical techDetail section when techDetail is absent
		?assertEqual(0, string:str(Result, "()")).

-endif.

buildContent(Subject, Sender, To, CurrentDate, MessageId, Body) ->
	"Subject: "++Subject++"\r\nFrom: "++Sender++"\r\nTo: <"++To++">"
	++"\r\nDate: "++CurrentDate++"\r\nMessage-ID: "++MessageId++"\r\n\r\n"++Body.

% RFC 5322 date-time format, e.g. "Mon, 20 Jul 2026 20:18:50 +0000". Mail relays
% (Gmail included) validate against this; RFC 3339 ("2026-07-20T20:18:50Z") is not
% the same format and isn't accepted for the Date: header.
formatRfc5322Date(UnixTime) ->
	{{Year, Month, Day}, {Hour, Min, Sec}} = calendar:system_time_to_universal_time(UnixTime, second),
	DayName = element(calendar:day_of_the_week(Year, Month, Day),
		{"Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"}),
	MonthName = element(Month,
		{"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"}),
	lists:flatten(io_lib:format("~s, ~2..0B ~s ~4..0B ~2..0B:~2..0B:~2..0B +0000",
		[DayName, Day, MonthName, Year, Hour, Min, Sec])).

% A globally-unique Message-ID, per RFC 5322 3.6.4. Gmail (and other RFC-compliant
% relays) reject messages that omit this header.
buildMessageId(Domain) ->
	lists:flatten(io_lib:format("<~B.~B@~s>",
		[erlang:system_time(second), erlang:unique_integer([positive]), Domain])).

getDomain(EmailAddress) ->
	case string:split(EmailAddress, "@") of
		[_, Domain] -> Domain;
		_ -> EmailAddress
	end.

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").

	buildContent_includes_message_id_test() ->
		Content = buildContent("Subject line", "Lucos Monitoring <monitoring@l42.eu>", "to@example.com",
			"Mon, 20 Jul 2026 20:18:50 +0000", "<12345.67@l42.eu>", "Body text"),
		?assert(string:str(Content, "\r\nMessage-ID: <12345.67@l42.eu>\r\n") > 0).

	buildContent_includes_date_test() ->
		Content = buildContent("Subject line", "Lucos Monitoring <monitoring@l42.eu>", "to@example.com",
			"Mon, 20 Jul 2026 20:18:50 +0000", "<12345.67@l42.eu>", "Body text"),
		?assert(string:str(Content, "\r\nDate: Mon, 20 Jul 2026 20:18:50 +0000\r\n") > 0).

	formatRfc5322Date_test() ->
		% 2026-07-20T20:18:50Z, a Monday
		?assertEqual("Mon, 20 Jul 2026 20:18:50 +0000", formatRfc5322Date(1784578730)).

	buildMessageId_format_test() ->
		MessageId = buildMessageId("l42.eu"),
		?assertMatch({match, _}, re:run(MessageId, "^<[0-9]+\\.[0-9]+@l42\\.eu>$")).

	buildMessageId_unique_test() ->
		?assertNotEqual(buildMessageId("l42.eu"), buildMessageId("l42.eu")).

	getDomain_test() ->
		?assertEqual("l42.eu", getDomain("monitoring@l42.eu")).

-endif.

sendEmail(Subject, Body) ->
	SendAddress =  os:getenv("SEND_ADDRESS"),
	Password = os:getenv("SEND_PASSWORD"),
	Relay = os:getenv("SMTP_RELAY"),
	To = os:getenv("TO_ADDRESS"),
	Sender = "Lucos Monitoring <"++SendAddress++">",
	CurrentDate = formatRfc5322Date(erlang:system_time(second)),
	MessageId = buildMessageId(getDomain(SendAddress)),
	Content = buildContent(Subject, Sender, To, CurrentDate, MessageId, Body),
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
				logger:error("Error Sending Email: ~p", [binary_to_list(ErrorMessage)]);
			{error, send, Error} ->
				logger:error("Unhandled Error Sending Email.  ~p", [Error]);
			{error, Type, Error} ->
				logger:error("Unhandled Email Error of Type ~p.  ~p", [Type, Error]);
			_ ->
				logger:error("Unexpected Response from sending email. ~p.", [Response])
		end
	end).