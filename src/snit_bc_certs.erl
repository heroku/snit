-module(snit_bc_certs).

-behaviour(snit_cert_store).
-behaviour(gen_server).

-export([
         start_link/0,
         add/2, update/2, upsert/2,
         delete/1,
         lookup/1,
         ready/0
        ]).

%% not sure if this should be test only or left in for maintenance
-export([set_wallet/1]).

%% gen_server API
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2, format_status/2]).

-define(DB, "certs-db").
-define(v1, 1:8).

-record(state,
        {
          ref,
          wallet
        }).

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
    gen_server:call(?MODULE, {lookup, Domain}).

ready() ->
    gen_server:call(?MODULE, ready).

set_wallet(Wallet) ->
    gen_server:call(?MODULE, {set_wallet, Wallet}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal Export / Callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
    Path = application:get_env(snit, certs_storage_path, "data/"),

    %% in the real world, this can fail
    Wallet = fetch_wallet(),

    %% do we want to tune other options here?  default file size maybe
    %% too big?
    case bitcask:open(Path ++ ?DB, [read_write]) of
        {error, Reason} ->
            Ref = undefined,
            exit({bc_open_error, Reason});
        Ref ->
            Ref
    end,
    {ok, #state{ ref = Ref, wallet = Wallet }}.

handle_call({set_wallet, Wallet}, _From, State) ->
    {reply, ok, State#state{wallet = Wallet}};
handle_call({add, Domain, Certs}, _From, #state{ref = Ref} = State) ->
    Reply =
        case snit_util:validate(Certs) of
            OK when OK =:= self_signed orelse
                    OK =:= valid ->
                case bitcask:get(Ref, Domain) of
                    {ok, _Value} ->
                        {error, already_exists};
                    not_found ->
                        case bitcask:put(Ref, Domain, serialize(Certs)) of
                            ok ->
                                ok;
                            %% may want to translate reasons for
                            %% better error messages.
                            {error, Reason} ->
                                {error, Reason}
                        end
                end;
            invalid ->
                {error, invalid_cert}
        end,
    {reply, Reply, State};
handle_call({update, Domain, Certs}, _From, #state{ref = Ref} = State) ->
    Reply =
        case snit_util:validate(Certs) of
            OK when OK =:= self_signed orelse
                    OK =:= valid ->
                case bitcask:get(Ref, Domain) of
                    {ok, _Value} ->
                        case bitcask:put(Ref, Domain, serialize(Certs)) of
                            ok ->
                                ok;
                            %% may want to translate reasons for
                            %% better error messages.
                            {error, Reason} ->
                                {error, Reason}
                        end;
                    not_found ->
                        {error, not_found}
                end;
            invalid ->
                {error, invalid_cert}
        end,
    {reply, Reply, State};
handle_call({upsert, Domain, Certs}, _From, #state{ref = Ref} = State) ->
    Reply =
        case snit_util:validate(Certs) of
            OK when OK =:= self_signed orelse
                    OK =:= valid ->
                BCerts = serialize(Certs),
                case bitcask:put(Ref, Domain, BCerts) of
                    ok ->
                        ok;
                    %% may want to translate reasons for
                    %% better error messages.
                    {error, Reason} ->
                        {error, Reason}
                end;
            invalid ->
                {error, invalid_cert}
        end,
    {reply, Reply, State};
handle_call({delete, Domain}, _From,  #state{ref = Ref} = State) ->
    Reply =
        case bitcask:delete(Ref, Domain) of
            ok ->
            ok;
            %% may want to translate reasons for
            %% better error messages.
            {error, Reason} ->
                {error, Reason}
        end,
    {reply, Reply, State};
handle_call({lookup, Domain}, _From,  #state{ref = Ref,
                                             wallet = Wallet} = State) ->
    Reply =
        case bitcask:get(Ref, Domain) of
            {ok, BCerts} ->
                {ok, deserialize(BCerts, Domain, Wallet)};
            %% may want to translate reasons for
            %% better error messages.
            {error, Reason} ->
                {error, Reason};
            not_found ->
                {error, not_found}
        end,
    {reply, Reply, State};
handle_call(_, _From, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state{ref = Ref}) ->
    _ = bitcask:close(Ref),
    ok.

format_status(_ServerState, [_Pdict, _State]) -> % sys:get_status or crashes
    [{data, [{"State", hidden}]}].

%%% Internal Functions

serialize(Certs) ->
    <<?v1, (term_to_binary(Certs))/binary>>.

deserialize(<<?v1, BCerts/binary>>, Domain, Wallet) ->
    KeyKey = snit_wallet:key(Domain, Wallet),
    Certs = binary_to_term(BCerts),
    EncKey = proplists:get_value(key, Certs),
    {ok, Key} = fernet:verify_and_decrypt_token(EncKey, KeyKey, infinity),
    lists:keyreplace(key, 1, Certs, {key, Key}).

fetch_wallet() ->
    snit_wallet:make_fake().
