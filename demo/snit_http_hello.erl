-module(snit_http_hello).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
    ok = ranch:accept_ack(Ref),
    loop(Socket, Transport).

loop(Socket, Transport) ->
    case Transport:recv(Socket, 0, 5000) of
        {ok, _} ->
            Transport:send(Socket, http()),
            loop(Socket, Transport);
        _ ->
            ok = Transport:close(Socket)
    end.

http() ->
    Page = page(),
    "HTTP/1.1 200 OK\r\n"
    "Content-Type: text/html\r\n"
    "Content-length: " ++ integer_to_list(length(Page)) ++ "\r\n"
    "\r\n" ++
    Page.

page() ->
    "<!DOCTYPE html>"
    "<html>"
     "<head><title>Test Page</title></head>"
     "<body>Hello crypto world</body>"
    "</html>".
