-module(snit_cert_store).

-opaque domain() :: binary().
-opaque certs() :: [{key, binary()} |
                    {cacert, binary()} |
                    {cert, binary()}, ...].

-export_type([domain/0, certs/0]).

-callback init_store([any()]) ->
    ok | {error, atom()}.

-callback add(domain(), certs(), term()) ->
    {ok, term()} |
    {{error, atom()}, term()}.

-callback update(domain(), certs(), term()) ->
    {ok, term()} |
    {{error, atom()}, term()}.

-callback upsert(domain(), certs(), term()) ->
    {ok, term()} |
    {{error, atom()}, term()}.

-callback delete(domain(), term()) ->
    {ok, term()} |
    {{error, atom()}, term()}.

-callback lookup(domain(), term()) ->
    {certs(), term()} |
    {{error, atom()}, term()}.

-callback terminate(term()) ->
    ok.

%% some comments from fred, earlier
%% delete_by_cert/1 <-- not supported until we need it
%% dump_to_file/1, restore_from_file/1 <-- Same

%% other stuff we probably need is a cred-roll mechanism, or at least
%% some generic callback for long-lived asynchronous operations over
%% the store

-behaviour(gen_server).

-export([
         start_link/2,
         add/2, update/2, upsert/2,
         delete/1,
         lookup/1
        ]).

%% not sure if this should be test only or left in for maintenance
-export([set_wallet/1]).

%% gen_server API
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2, format_status/2]).

-record(state,
        {
          mod,
          mod_state,
          wallet
        }).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public Interface %%%
%%%%%%%%%%%%%%%%%%%%%%%%
start_link(Mod, Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Mod, Args], []).

add(Domain, Certs) ->
    validate_call(Domain, Certs, add).

update(Domain, Certs) ->
    validate_call(Domain, Certs, update).

upsert(Domain, Certs) ->
    validate_call(Domain, Certs, upsert).

validate_call(Domain, Certs, Op) ->
    case snit_util:validate(Certs) of
        OK when OK =:= self_signed orelse
                OK =:= valid ->
            gen_server:call(?MODULE, {Op, Domain, Certs});
        invalid ->
            {error, invalid_cert}
    end.

delete(Domain) ->
    gen_server:call(?MODULE, {delete, Domain}).

lookup(Domain) ->
    gen_server:call(?MODULE, {lookup, Domain}).

set_wallet(Wallet) ->
    gen_server:call(?MODULE, {set_wallet, Wallet}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal Export / Callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([Mod, Args]) ->
    process_flag(sensitive, true), % keeps people from snooping
    Wallet = case proplists:get_value(encrypted, Args, true) of
                 true ->
                     fetch_wallet();
                 false ->
                     undefined
             end,

    case Mod:init_store(Args) of
        {ok, ModState} ->
            {ok, #state{ mod = Mod,
                         mod_state = ModState,
                         wallet = Wallet }};
        {error, Reason} ->
            {error, Reason}
    end.

handle_call({set_wallet, Wallet}, _From, State) ->
    {reply, ok, State#state{wallet = Wallet}};
handle_call({add, Domain, Certs}, _From,
            #state{mod = Mod, mod_state = ModState1} = State) ->
    {Reply, ModState} =
        case Mod:add(Domain, Certs, ModState1) of
            {ok, ModState0} ->
                {ok, ModState0};
            {{error, Reason}, ModState0} ->
                {{error, Reason}, ModState0}
        end,
    {reply, Reply, State#state{mod_state = ModState}};
handle_call({update, Domain, Certs}, _From,
            #state{mod = Mod, mod_state = ModState1} = State) ->
    {Reply, ModState} =
        case Mod:update(Domain, Certs, ModState1) of
            {ok, ModState0} ->
                {ok, ModState0};
            {{error, Reason}, ModState0} ->
                {{error, Reason}, ModState0}
        end,
    {reply, Reply, State#state{mod_state = ModState}};
handle_call({upsert, Domain, Certs}, _From,
            #state{mod = Mod, mod_state = ModState1} = State) ->
    {Reply, ModState} =
        case Mod:upsert(Domain, Certs, ModState1) of
            {ok, ModState0} ->
                {ok, ModState0};
            {{error, Reason}, ModState0} ->
                {{error, Reason}, ModState0}
        end,
    {reply, Reply, State#state{mod_state = ModState}};
handle_call({delete, Domain}, _From,
            #state{mod = Mod, mod_state = ModState1} = State) ->
    {Reply, ModState} =
        case Mod:delete(Domain, ModState1) of
            {ok, ModState0} ->
                {ok, ModState0};
            {{error, Reason}, ModState0} ->
                {{error, Reason}, ModState0}
        end,
    {reply, Reply, State#state{mod_state = ModState}};
handle_call({lookup, Domain}, _From,
            #state{mod = Mod, mod_state = ModState1,
                   wallet = undefined} = State) ->
    {Reply, ModState} =
        case Mod:lookup(Domain, ModState1) of
            {{ok, Value}, ModState0} ->
                {Value, ModState0};
            {{error, Reason}, ModState0} ->
                {{error, Reason}, ModState0}
        end,
    {reply, Reply, State#state{mod_state = ModState}};
handle_call({lookup, Domain}, _From,
            #state{mod = Mod, mod_state = ModState1,
                   wallet = Wallet} = State) ->
    {Reply, ModState} =
        case Mod:lookup(Domain, ModState1) of
            {{ok, Value0}, ModState0} ->
                case decrypt(Value0, Domain, Wallet) of
                    {ok, Value} ->
                        {Value, ModState0};
                    {error, Reason} ->
                        {{error, Reason}, ModState0}
                end;
            {{error, Reason}, ModState0} ->
                {{error, Reason}, ModState0}
        end,
    {reply, Reply, State#state{mod_state = ModState}};
handle_call(_, _From, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state{mod=Mod, mod_state = ModState}) ->
    _ = Mod:terminate(ModState),
    ok.

format_status(_ServerState, [_Pdict, _State]) -> % sys:get_status or crashes
    [{data, [{"State", hidden}]}].

%%% Internal Functions

decrypt(Certs, Domain, Wallet) ->
    KeyKey = snit_wallet:key(Domain, Wallet),
    {KeyType, EncKey} = proplists:get_value(key, Certs),
    {ok, Key} = fernet:verify_and_decrypt_token(EncKey, KeyKey, infinity),
    %% wrap in OK here to keep apis in place for future error checking
    {ok, lists:keyreplace(key, 1, Certs, {key, {KeyType, Key}})}.

fetch_wallet() ->
    snit_wallet:make_fake().
