-module(snit_store_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

groups() ->
    [].

all() ->
    [].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(snit),
    lager_common_test_backend:bounce(info),
    application:set_env(snit, certs_storage_path,
                        ?config(data_dir, Config) ++ "/data/"),
    Config.

end_per_suite(_Config) ->
    application:stop(snit),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(in_mem_connect, Config) ->
    snit:start(connect_sni, 2, 8001, fun test_sni_fun_mem/1, snit_echo, []),
    {ok, Cert0} = file:read_file(?config(data_dir, Config) ++ "cert.pem"),
    [{_, Cert,_}] = public_key:pem_decode(Cert0),
    snit_certs:add("memhost", [{cert, Cert}]),
    Config;
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_, Config) ->
    ok.


%%% helper functions


