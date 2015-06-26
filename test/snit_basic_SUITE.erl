-module(snit_basic_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

groups() ->
	[].

all() ->
	[connect, connect_sni, proxy, in_mem_connect, update, null].

init_per_suite(Config) ->
	{ok, _} = application:ensure_all_started(snit),
	lager_common_test_backend:bounce(info),
	Config.

end_per_suite(_Config) ->
	application:stop(snit),
	ok.

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, _Config) ->
	ok.

init_per_testcase(connect, Config) ->
	snit:start(connect, 2, 8001, fun test_sni_fun/1, snit_echo, []),
	snit_ets_certs:add("localhost", [{cacertfile, ?config(data_dir, Config) ++ "cacerts.pem"},
									 {certfile, ?config(data_dir, Config) ++ "cert.pem"},
									 {keyfile, ?config(data_dir, Config) ++ "key.pem"}]),
	Config;
init_per_testcase(connect_sni, Config) ->
	snit:start(connect_sni, 2, 8001, fun test_sni_fun/1, snit_echo, []),
	snit_ets_erts:add("snihost", [{cacertfile, ?config(data_dir, Config) ++ "cacerts.pem"},
								  {certfile, ?config(data_dir, Config) ++ "cert.pem"},
								  {keyfile, ?config(data_dir, Config) ++ "key.pem"}]),
	Config;
init_per_testcase(in_mem_connect, Config) ->
	snit:start(in_mem_connect, 2, 8001, fun test_sni_fun_mem/1, snit_echo, []),
	{ok, Cert0} = file:read_file(?config(data_dir, Config) ++ "cert.pem"),
	[{_, Cert,_}] = public_key:pem_decode(Cert0),
	snit_ets_certs:add("memhost", [{cert, Cert}]),
	Config;
init_per_testcase(proxy, Config) ->
	{ok, Listen} = gen_tcp:listen(0, [{active, false}]),
	{ok, {{0,0,0,0}, Port}} = inet:sockname(Listen),
	IP = {127,0,0,1},
	Backend = spawn_link(fun() -> accept(Listen) end),
	snit:start(proxy, 2, 8001, fun test_sni_fun/1, snit_tcp_proxy, [{dest, {IP,Port}}]),
	snit_ets_certs:add("snihost", [{cacertfile, ?config(data_dir, Config) ++ "cacerts.pem"},
                                   {certfile, ?config(data_dir, Config) ++ "cert.pem"},
                                   {keyfile, ?config(data_dir, Config) ++ "key.pem"}]),
	[{backends, [Backend]} | Config];
init_per_testcase(update, Config) ->
	{ok, Listen} = gen_tcp:listen(0, [{active, false}]),
	{ok, {{0,0,0,0}, Port}} = inet:sockname(Listen),
	IP = {127,0,0,1},
	Backends = [spawn_link(fun() -> accept(Listen) end) || _ <- [1,2]],
	snit:start(update, 2, 8001, fun test_sni_fun/1, snit_tcp_proxy, [{dest, {IP,Port}}]),
	snit_ets_certs:add("update", [{cacertfile, ?config(data_dir, Config) ++ "cacerts.pem"},
                                  {certfile, ?config(data_dir, Config) ++ "cert.pem"},
                                  {keyfile, ?config(data_dir, Config) ++ "key.pem"}]),
	[{backends, Backends} | Config];
init_per_testcase(null, Config) ->
	{ok, Listen} = gen_tcp:listen(0, [{active, false}]),
	{ok, {{0,0,0,0}, Port}} = inet:sockname(Listen),
	IP = {127,0,0,1},
	Backends = [spawn_link(fun() -> accept(Listen) end) || _ <- [1,2]],
	snit:start(null, 2, 8001, fun test_sni_fun/1, snit_tcp_proxy, [{dest, {IP,Port}}]),
	snit_ets_certs:add("null", [{cacertfile, ?config(data_dir, Config) ++ "cacerts.pem"},
                                {certfile, ?config(data_dir, Config) ++ "cert.pem"},
                                {keyfile, ?config(data_dir, Config) ++ "key.pem"}]),
	[{backends, Backends} | Config];
init_per_testcase(_TestCase, Config) ->
	Config.

end_per_testcase(connect, _Config) ->
	snit:stop(connect),
	snit_ets_certs:delete("localhost"),
    timer:sleep(1000),
	ok;
end_per_testcase(connect_sni, _Config) ->
	snit:stop(connect_sni),
	snit_ets_certs:delete("snihost"),
	ok;
end_per_testcase(in_mem_connect, _Config) ->
	snit:stop(in_mem_connect),
	snit_ets_certs:delete("memhost"),
	ok;
end_per_testcase(proxy, Config) ->
	snit:stop(proxy),
	[shutdown(Pid) || Pid <- ?config(backends, Config)],
	snit_ets_certs:delete("snihost"),
	ok;
end_per_testcase(update, Config) ->
	snit:stop(update),
	[shutdown(Pid) || Pid <- ?config(backends, Config)],
	snit_ets_certs:delete("update"),
	ok;
end_per_testcase(null, Config) ->
	snit:stop(null),
	[shutdown(Pid) || Pid <- ?config(backends, Config)],
	snit_ets_certs:delete("null"),
	ok.

connect(Config) ->
	{ok, S} = ssl:connect("localhost", 8001, [{active, true},binary]),
	ssl:send(S, <<"first">>),
	receive
		{ssl, S, <<"first">>} ->
			ok;
		Else ->
			lager:info("else ~p", [Else]),
			error(unexpected_message)
	after 500 ->
			error(timeout)
	end,
	Config.

connect_sni(Config) ->
	{ok, S} = ssl:connect("localhost", 8001,
						  [{active, true},binary,
						   {server_name_indication, "snihost"}]),
	ssl:send(S, <<"first">>),
	receive
		{ssl, S, <<"first">>} ->
			ok;
		Else ->
			lager:info("else ~p", [Else]),
			error(unexpected_message)
	after 500 ->
			error(timeout)
	end,
	Config.

in_mem_connect(Config) ->
	{ok, S} = ssl:connect("localhost", 8001,
						  [{active, true},binary,
						   {server_name_indication, "memhost"}]),
	ssl:send(S, <<"first">>),
	receive
		{ssl, S, <<"first">>} ->
			ok;
		Else ->
			lager:info("else ~p", [Else]),
			error(unexpected_message)
	after 500 ->
			error(timeout)
	end,
	Config.

proxy(Config) ->
	{ok, S} = ssl:connect("localhost", 8001,
						  [{active, true},binary,
						   {server_name_indication, "snihost"}]),
	ssl:send(S, <<"first">>),
	receive
		{ssl, S, <<"returned: first">>} ->
			ok;
		Else ->
			lager:info("else ~p", [Else]),
			error({unexpected_message, Else})
	after 500 ->
			error(timeout)
	end,
	Config.

update(Config) ->
	{ok, S1} = ssl:connect("localhost", 8001,
						   [{active, true},binary,
							{server_name_indication, "update"}]),
	snit_ets_certs:update("update", [{certfile, ?config(data_dir, Config) ++ "selfsigned.crt"},
                                     {keyfile, ?config(data_dir, Config) ++ "selfsigned.key"}]),
	ssl:send(S1, <<"first">>),
	{ok, S2} = ssl:connect("localhost", 8001,
						   [{active, true},binary,
							{server_name_indication, "update"}]),
	ssl:send(S2, <<"second">>),
	receive
		{ssl, S, <<"returned: ", Msg1/binary>>} ->
			Other = case S of
                        S1 -> S2;
                        S2 -> S1
                    end,
			receive
				{ssl, Other, <<"returned: ", Msg2/binary>>} ->
					[<<"first">>, <<"second">>] = lists:sort([Msg1, Msg2]),
					ok;
				Else ->
					lager:info("else ~p", [Else]),
					error({unexpected_message, Else})
			after 500 ->
					error(timeout)
			end;
		Else ->
			lager:info("else ~p", [Else]),
			error({unexpected_message, Else})
	after 500 ->
			error(timeout)
	end,
	Config.

null(Config) ->
	{ok, S} = ssl:connect("localhost", 8001,
						  [{active, true},binary,
						   {server_name_indication, "null"}]),
	snit_ets_certs:delete("null"),
	%% The cert is gone, this one fails.
	{error, _} = ssl:connect("localhost", 8001,
                             [{active, true},binary,
                              {server_name_indication, "null"}]),
	%% Still valid for the in-flight session
	ssl:send(S, <<"first">>),
	receive
		{ssl, S, <<"returned: first">>} ->
			ok;
		Else ->
			lager:info("else ~p", [Else]),
			error(unexpected_message)
	after 500 ->
			error(timeout)
	end,
	Config.

test_sni_fun(SNIHostname) ->
	lager:debug("sni hostname: ~p", [SNIHostname]),
	snit_ets_certs:lookup(SNIHostname).

test_sni_fun_mem(SNIHostname) ->
	lager:debug("sni hostname: ~p", [SNIHostname]),
	snit_ets_certs:lookup(SNIHostname).

accept(Listen) ->
	{ok, Sock} = gen_tcp:accept(Listen),
	loop(Sock).

loop(Sock) ->
	case gen_tcp:recv(Sock, 0, 5000) of
		{ok, Data} ->
			lager:info("tcp test loop: ~p", [Data]),
			ok = gen_tcp:send(Sock, ["returned: ", Data]),
			loop(Sock);
		{error, closed} ->
			exit(normal)
	end.

shutdown(undefined) -> ok;
shutdown(Pid) ->
	unlink(Pid),
	Ref = erlang:monitor(process, Pid),
	exit(Pid, shutdown),
	receive
		{'DOWN', Ref, process, Pid, _} -> ok
	after
		5000 -> error(shutdown_timeout)
	end.
