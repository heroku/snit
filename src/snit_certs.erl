-module(snit_certs).
-behaviour(gen_server).
-export([start_link/0,
         add/2, update/2, upsert/2, delete/1,
        % delete_by_cert/1 <-- not supported until we need it
        % dump_to_file/1, restore_from_file/1 <-- Same
         lookup/1]).
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2, format_status/2]).

-define(TABLE, ?MODULE).
-record(state, {}).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public Interface %%%
%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add(Domain, Certs) ->
    gen_server:call(?MODULE, {add, Domain, Certs}).

update(Domain, Certs) ->
    gen_server:call(?MODULE, {update, Domain, Certs}).

upsert(Domain, Certs) ->
    gen_server:call(?MODULE, {upsert, Domain, Certs}).

delete(Domain) ->
    gen_server:call(?MODULE, {delete, Domain}).

lookup(Domain) ->
    try ets:lookup(?TABLE, Domain) of
        [{_, Opts}] -> Opts;
        [] -> []
    catch
        error:badarg -> % table is down
            []
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal Export / Callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
    process_flag(sensitive, true), % keeps people from snooping, but the table is readable from other procs :/
    ets:new(?TABLE, [protected, set, named_table, {read_concurrency, true}]),
    {ok, #state{}}.

handle_call({add, Domain, Certs}, _From, State) ->
    Reply = case ets:insert_new(?TABLE, {Domain, Certs}) of
        true -> ok;
        false -> {error, already_exists}
    end,
    {reply, Reply, State};
handle_call({update, Domain, Certs}, _From, State) ->
    Reply = case ets:lookup(?TABLE, Domain) of
        [] -> {error, not_found};
        [_] ->
            ets:insert(?TABLE, {Domain, Certs}),
            ok
    end,
    {reply, Reply, State};
handle_call({upsert, Domain, Certs}, _From, State) ->
    ets:insert(?TABLE, {Domain, Certs}),
    {reply, ok, State};
handle_call({delete, Domain}, _From, State) ->
    ets:delete(?TABLE, Domain),
    {reply, ok, State};
handle_call(_, _From, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

format_status(_ServerState, [_Pdict, _State]) -> % sys:get_status or crashes
    [{data, [{"State", hidden}]}].
