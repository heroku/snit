-module(snit_bc_store).

-behaviour(snit_cert_store).

-export([
         init_store/1,
         add/3, update/3, upsert/3,
         delete/2,
         lookup/2,
         terminate/1
        ]).

-define(DB, "certs-db").
-define(v1, 1:8).

-type snit_bc_store_state() :: reference().
-export_type([snit_bc_store_state/0]).

%%% behavior callbacks
-spec init_store([any()]) -> {ok, snit_bc_store_state(), boolean()}
                           | {error, atom()}.
init_store(_) ->
    Path = application:get_env(snit, certs_storage_path, "data/"),

    %% do we want to tune other options here?  default file size maybe
    %% too big?  note also that this operation can block for a while
    %% with huge data sets, we may need to increase the default start
    %% timeout of the gen_server maybe?
    case bitcask:open(Path ++ ?DB, [read_write]) of
        {error, Reason} ->
            {error, Reason};
        Ref ->
            {ok, Ref, true}
    end.

add(Domain, Certs, Ref) ->
    Reply =
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
        end,
    {Reply, Ref}.

update(Domain, Certs, Ref) ->
    Reply =
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
        end,
    {Reply, Ref}.

upsert(Domain, Certs, Ref) ->
    BCerts = serialize(Certs),
    Reply =
        case bitcask:put(Ref, Domain, BCerts) of
            ok ->
                ok;
            %% may want to translate reasons for
            %% better error messages.
            {error, Reason} ->
                {error, Reason}
        end,
    {Reply, Ref}.

delete(Domain, Ref) ->
    Reply =
        try bitcask:delete(Ref, Domain) of
            ok ->
                ok
        catch
            %% bitcask:delete always returns 'ok', but could
            %% throw({error, Reason}). We may want to translate
            %% Reason for better error messages.
            {error, Reason} ->
                {error, Reason}
        end,
    {Reply, Ref}.

lookup(Domain, Ref) ->
    Reply =
        case bitcask:get(Ref, Domain) of
            {ok, BCerts} ->
                deserialize(BCerts);
            %% may want to translate reasons for
            %% better error messages.
            {error, Reason} ->
                {error, Reason};
            not_found ->
                {error, not_found}
        end,
    {Reply, Ref}.

terminate(Ref) ->
    _ = bitcask:close(Ref).

%%% Internal Functions

serialize(Certs) ->
    <<?v1, (term_to_binary(Certs))/binary>>.

deserialize(<<?v1, BCerts/binary>>) ->
    try
        Certs = binary_to_term(BCerts),
        {ok, Certs}
    catch _:_ ->
            {error, corrupted_certs}
    end.
