-module(snit_test_cert_store).

-behaviour(gen_server).

%% API
-export([
         start/0,
         add/2,
         delete/1,
         update/2,
         lookup/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

add(Domain, Certs) ->
    gen_server:call(?MODULE, {add, Domain, Certs}).

delete(Domain) ->
    gen_server:call(?MODULE, {delete, Domain}).

update(Domain, Certs) ->
    gen_server:call(?MODULE, {update, Domain, Certs}).

lookup(Domain) ->
    gen_server:call(?MODULE, {lookup, Domain}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, []}.

handle_call({add, Domain, Certs}, _From, State) ->
    {reply, ok, [{Domain, Certs}|State]};
handle_call({update, Domain, Certs}, _From, State) ->
    {reply, ok, [{Domain, Certs}|lists:keydelete(Domain, 1, State)]};
handle_call({delete, Domain}, _From, State) ->
    {reply, ok, lists:keydelete(Domain, 1, State)};
handle_call({lookup, Domain}, _From, State) ->
    case lists:keyfind(Domain, 1, State) of
        {Domain, Certs} ->
            {reply, Certs, State};
        false ->
            {reply, {error, not_found}, State}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
