-module(snit_ets_store).

-behaviour(snit_cert_store).

-export([
         init_store/1,
         add/3, update/3, upsert/3,
         delete/2,
         lookup/2,
         terminate/1
        ]).

-define(TABLE, ?MODULE).
-define(STATE, ?MODULE).
-define(SR, #?STATE).
-record(?STATE, {}).
-type snit_ets_store_state() :: ?SR{}.
-export_type([snit_ets_store_state/0]).

%%% behavior callbacks

init_store(Args) ->
    Encrypted = proplists:get_value(encrypted, Args, true),
    ets:new(?TABLE, [private, set, named_table, {read_concurrency, true}]),
    {ok, ?SR{}, Encrypted}.

add(Domain, Certs, State) ->
    Reply = case ets:insert_new(?TABLE, {Domain, Certs}) of
        true -> ok;
        false -> {error, already_exists}
    end,
    {Reply, State}.

update(Domain, Certs, State) ->
    Reply = case ets:lookup(?TABLE, Domain) of
        [] -> {error, not_found};
        [_] ->
            ets:insert(?TABLE, {Domain, Certs}),
            ok
    end,
    {Reply, State}.

upsert(Domain, Certs, State) ->
    ets:insert(?TABLE, {Domain, Certs}),
    {ok, State}.

delete(Domain, State) ->
    ets:delete(?TABLE, Domain),
    {ok, State}.

lookup(Domain, State) ->
    Reply =
        try ets:lookup(?TABLE, Domain) of
            [{_, Opts}] -> {ok, Opts};
            [] -> {error, not_found}
        catch
            error:badarg -> % table is down
                {error, not_found}
        end,
    {Reply, State}.

terminate(_State) ->
    %% shouldn't need to terminate ets store as it's owned by this process.
    ok.
