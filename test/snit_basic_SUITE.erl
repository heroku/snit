-module(snit_basic_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

groups() ->
    [].

all() ->
    [connect].

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
    snit:start(connect, 2, 8001, fun test_sni_fun/1),
    ets:new(test_tab, [public, named_table]),
    ets:insert(test_tab, {"localhost", {?config(data_dir, Config) ++ "cacerts.pem",
                                        ?config(data_dir, Config) ++ "cert.pem",
                                        ?config(data_dir, Config) ++ "key.pem"}}
              ),
    Config;
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
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

test_sni_fun(SNIHostname) ->
    lager:debug("sni hostname: ~p", [SNIHostname]),
    [{_, {CaCertFile, CertFile, KeyFile}}] = ets:lookup(test_tab, SNIHostname),
    [{cacertfile, CaCertFile}, {certfile, CertFile}, {keyfile, KeyFile}].



