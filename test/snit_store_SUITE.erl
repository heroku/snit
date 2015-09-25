-module(snit_store_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

groups() ->
    [].

all() ->
    [basic].

init_per_suite(Config) ->
    random:seed(erlang:unique_integer()),
    application:set_env(snit, certs_storage_path,
                        ?config(data_dir, Config) ++ "/data/"),
    application:set_env(snit, store_type, bitcask),
    {ok, _} = application:ensure_all_started(snit),
    lager_common_test_backend:bounce(info),
    Config.

end_per_suite(Config) ->
    os:cmd("rm -r " ++ ?config(data_dir, Config) ++ "/data"),
    application:stop(snit),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(basic, Config) ->
    N = 250,
    {Domains, Wallet} = setup(N, Config),
    snit_cert_store:set_wallet(Wallet),
    [{domains, Domains},
     {n, N}
     |Config];
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

basic(Config) ->
    Domains = ?config(domains, Config),
    N = ?config(n, Config),
    [begin
         I = random:uniform(N),
         {Domain, Certs} = lists:nth(I, Domains),
         %% ct:pal("checking ~p", [Domain]),
         {key, PlainKey} = lists:keyfind(key, 1, Certs),
         StoreCerts = snit_cert_store:lookup(Domain),
         %% repeat match below intentional
         {key, PlainKey} = lists:keyfind(key, 1, StoreCerts)
     end
     || _ <- lists:seq(1, 1000)].

%%% helper functions

setup(Num, Config) ->
    Wallet = snit_wallet:make_fake(),
    Domains = generate_pairs(Num, Config),
    [add(Domain, Certs, Wallet)
     || {Domain, Certs} <- Domains],
    {Domains, Wallet}.

generate_pairs(Num, Config) ->
    [{crypto:rand_bytes(32),
      load_mem_certs(Config)}
     || _ <- lists:seq(1, Num)].

add(Domain, Certs0, Wallet) ->
    Key = snit_wallet:key(Domain, Wallet),
    {KeyType, DecKey} = proplists:get_value(key, Certs0),
    EncKey = fernet:generate_token(DecKey, Key),
    EncTuple = {KeyType, EncKey},
    Certs = lists:keyreplace(key, 1, Certs0, {key, EncTuple}),
    %% ct:pal("adding ~p", [Domain]),
    ok = snit_cert_store:add(Domain, Certs).

load_mem_certs(Config) ->
    case random:uniform(20) of
        %% selfsigned
        1 ->
            {ok, SSCert0} = file:read_file(?config(data_dir, Config) ++ "selfsigned.key"),
            [{_, SSCert, _}] = public_key:pem_decode(SSCert0),
            {ok, SSKey0} = file:read_file(?config(data_dir, Config) ++ "selfsigned.key"),
            [{KeyType, SSKey, _}] = public_key:pem_decode(SSKey0),
            [{cert, SSCert}, {key, {KeyType, SSKey}}];
        _ ->
            {ok, Cert0} = file:read_file(?config(data_dir, Config) ++ "cert.pem"),
            [{_, Cert, _}] = public_key:pem_decode(Cert0),
            {ok, CaCert0} = file:read_file(?config(data_dir, Config) ++ "cacerts.pem"),
            CaCert1 = public_key:pem_decode(CaCert0),
            CaCert = [CCert || {_, CCert, _} <- CaCert1],
            {ok, Key0} = file:read_file(?config(data_dir, Config) ++ "key.pem"),
            [{KeyType, Key, _}] = public_key:pem_decode(Key0),
            [{cert, Cert}, {cacerts, CaCert}, {key, {KeyType, Key}}]
    end.
