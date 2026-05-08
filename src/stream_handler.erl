-module(stream_handler).
-export([start/0, subscribe/1, make_publish_fun/0]).

% Starts the stream handler process and registers it under the atom 'stream_handler'.
% The handler maintains a list of subscriber PIDs (one per connected SSE client).
start() ->
	Pid = spawn(fun() -> loop([]) end),
	register(stream_handler, Pid),
	Pid.

% Returns a fun that, when called with a system list, renders the dashboard block
% and fans it out to all connected SSE clients.
% Intended to be injected into monitoring_state_server as the publish hook.
make_publish_fun() ->
	fun(SystemList) ->
		Html = view:render_dashboard_block(SystemList),
		stream_handler ! {publish, Html}
	end.

% Called from server.erl after sending SSE response headers.
% Registers the calling process as a subscriber, then enters a receive loop
% that writes SSE events to the socket and sends heartbeats every ~25s.
% This call blocks until the client disconnects.
subscribe(Socket) ->
	stream_handler ! {subscribe, self()},
	subscriber_loop(Socket).

% Per-client receive loop: waits for {sse_event, Html} messages from the handler
% or sends a heartbeat comment after 25s of inactivity.
subscriber_loop(Socket) ->
	receive
		{sse_event, Html} ->
			SsePacket = format_sse_data(Html),
			case gen_tcp:send(Socket, SsePacket) of
				ok ->
					subscriber_loop(Socket);
				_ ->
					stream_handler ! {unsubscribe, self()},
					gen_tcp:close(Socket)
			end
	after 25000 ->
		% SSE comment heartbeat — defeats nginx proxy_read_timeout (default 60s)
		case gen_tcp:send(Socket, ": ping\n\n") of
			ok ->
				subscriber_loop(Socket);
			_ ->
				stream_handler ! {unsubscribe, self()},
				gen_tcp:close(Socket)
		end
	end.

% The stream handler process loop. Maintains a list of subscriber PIDs.
% On publish, fans out the HTML to all live subscribers.
loop(Clients) ->
	receive
		{subscribe, Pid} ->
			loop([Pid | Clients]);
		{unsubscribe, Pid} ->
			loop(lists:delete(Pid, Clients));
		{publish, Html} ->
			AliveClients = [Pid || Pid <- Clients, is_process_alive(Pid)],
			lists:foreach(fun(Pid) -> Pid ! {sse_event, Html} end, AliveClients),
			loop(AliveClients)
	end.

% Formats an HTML fragment as an SSE data event.
% Each line of the HTML becomes a 'data: <line>' SSE field line.
% The event is terminated by a blank line (\n\n).
format_sse_data(Html) ->
	Prefixed = re:replace(Html, "\n", "\ndata: ", [global, {return, list}]),
	"data: " ++ Prefixed ++ "\n\n".

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").

	format_sse_data_single_line_test() ->
		Result = format_sse_data("<div>hello</div>"),
		?assertEqual("data: <div>hello</div>\n\n", Result).

	format_sse_data_multi_line_test() ->
		Result = format_sse_data("<div>\nhello\n</div>"),
		?assertEqual("data: <div>\ndata: hello\ndata: </div>\n\n", Result).

	format_sse_data_empty_test() ->
		Result = format_sse_data(""),
		?assertEqual("data: \n\n", Result).

-endif.
