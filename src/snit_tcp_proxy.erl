-module(snit_tcp_proxy).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, [{dest, {IP,Port}}]) ->
    ok = ranch:accept_ack(Ref),
    {ok, TCP} = gen_tcp:connect(IP, Port, [{active, false}, {nodelay,true}]),
    snit_bytepipe:become({Transport, Socket}, {ranch_tcp, TCP}).
