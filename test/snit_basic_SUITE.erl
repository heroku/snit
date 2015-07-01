-module(snit_basic_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

groups() ->
	[].

all() ->
	[connect, connect_sni, proxy, update, null].

%% run this with both?
init_per_suite(Config) ->
    random:seed(erlang:unique_integer()),
    lager_common_test_backend:bounce(info),
    {ok, _} = application:ensure_all_started(snit),
    Wallet = snit_wallet:make_fake(),
    snit_cert_store:set_wallet(Wallet),
    [{wallet, Wallet} | Config].

end_per_suite(_Config) ->
	application:stop(snit),
	ok.

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, _Config) ->
	ok.

init_per_testcase(connect, Config) ->
	snit:start(connect, 2, 8001, fun test_sni_fun/1, snit_echo, []),
	load("localhost", Config),
	Config;
init_per_testcase(connect_sni, Config) ->
	snit:start(connect_sni, 2, 8001, fun test_sni_fun/1, snit_echo, []),
    load("snihost", Config),
	Config;
init_per_testcase(proxy, Config) ->
	{ok, Listen} = gen_tcp:listen(0, [{active, false}]),
	{ok, {{0,0,0,0}, Port}} = inet:sockname(Listen),
	IP = {127,0,0,1},
	Backend = spawn_link(fun() -> accept(Listen) end),
	snit:start(proxy, 2, 8001, fun test_sni_fun/1, snit_tcp_proxy, [{dest, {IP,Port}}]),
    load("snihost", Config),
	[{backends, [Backend]} | Config];
init_per_testcase(update, Config) ->
	{ok, Listen} = gen_tcp:listen(0, [{active, false}]),
	{ok, {{0,0,0,0}, Port}} = inet:sockname(Listen),
	IP = {127,0,0,1},
	Backends = [spawn_link(fun() -> accept(Listen) end) || _ <- [1,2]],
	snit:start(update, 2, 8001, fun test_sni_fun/1, snit_tcp_proxy, [{dest, {IP,Port}}]),
    load("update", Config),
	[{backends, Backends} | Config];
init_per_testcase(null, Config) ->
	{ok, Listen} = gen_tcp:listen(0, [{active, false}]),
	{ok, {{0,0,0,0}, Port}} = inet:sockname(Listen),
	IP = {127,0,0,1},
	Backends = [spawn_link(fun() -> accept(Listen) end) || _ <- [1,2]],
	snit:start(null, 2, 8001, fun test_sni_fun/1, snit_tcp_proxy, [{dest, {IP,Port}}]),
	load("null", Config),
	[{backends, Backends} | Config];
init_per_testcase(_TestCase, Config) ->
	Config.

load(Domain, Config) ->
    KeyKey = snit_wallet:key(Domain, ?config(wallet, Config)),
    {ok, Cert0} = file:read_file(?config(data_dir, Config) ++ "cert.pem"),
    [{_, Cert, _}] = public_key:pem_decode(Cert0),
    {ok, CaCert0} = file:read_file(?config(data_dir, Config) ++ "cacerts.pem"),
    CaCert1 = public_key:pem_decode(CaCert0),
    CaCert = [CCert || {_, CCert, _} <- CaCert1],
    {ok, Key1} = file:read_file(?config(data_dir, Config) ++ "key.pem"),
    [{KeyType, Key0, _}] = public_key:pem_decode(Key1),
    Key = fernet:generate_token(Key0, KeyKey),
    Certs = [{cert, Cert}, {cacerts, CaCert}, {key, {KeyType, Key}}],
    snit_cert_store:add(Domain, Certs).

end_per_testcase(connect, _Config) ->
	snit:stop(connect),
	snit_cert_store:delete("localhost"),
    timer:sleep(1000),
	ok;
end_per_testcase(connect_sni, _Config) ->
	snit:stop(connect_sni),
	snit_cert_store:delete("snihost"),
	ok;
end_per_testcase(proxy, Config) ->
	snit:stop(proxy),
	[shutdown(Pid) || Pid <- ?config(backends, Config)],
	snit_cert_store:delete("snihost"),
	ok;
end_per_testcase(update, Config) ->
	snit:stop(update),
	[shutdown(Pid) || Pid <- ?config(backends, Config)],
	snit_cert_store:delete("update"),
	ok;
end_per_testcase(null, Config) ->
	snit:stop(null),
	[shutdown(Pid) || Pid <- ?config(backends, Config)],
	snit_cert_store:delete("null"),
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
	snit_cert_store:update("update", [{certfile, ?config(data_dir, Config) ++ "selfsigned.crt"},
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
	snit_cert_store:delete("null"),
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
	case snit_cert_store:lookup(SNIHostname) of
        {ok, Certs} ->
            Certs;
        {error, not_found} ->
            []
    end.

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
