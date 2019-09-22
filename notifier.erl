-module(notifier).
-export([notify/4]).


notify(Host, SystemName, SystemChecks, SystemMetrics) ->
	System = getSystemTitle(Host, SystemName),
	sendEmail("Monitoring Test", "This is the email body 5"),
	io:format("Send notifications for ~p~n", [System]).


getSystemTitle(Host, Name) ->
	case Name of
		unknown ->
			Host;
		_ ->
			Name
	end.

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