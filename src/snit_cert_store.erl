-module(snit_cert_store).

-opaque domain() :: binary().
-opaque certs() :: [{key, binary()} |
                    {cacert, binary()} |
                    {cert, binary()}, ...].

-export_type([domain/0, certs/0]).

-callback add(domain(), certs()) ->
    ok | {error, atom()}.

-callback update(domain(), certs()) ->
    ok | {error, atom()}.

-callback upsert(domain(), certs()) ->
    ok | {error, atom()}.

-callback delete(domain()) ->
    ok | {error, atom()}.

-callback lookup(domain()) ->
    certs().

-callback ready() ->
    boolean().

%% some comments from fred, earlier
%% delete_by_cert/1 <-- not supported until we need it
%% dump_to_file/1, restore_from_file/1 <-- Same

%% other stuff we probably need is a cred-roll mechanism, or at least
%% some generic callback for long-lived asynchronous operations over
%% the store


%% not sure if we should just move the gen_server in here, and
%% simplify the callback?  I think that's the right move if this sees
%% production or the number of certs providers rises above 2 (or if
%% one moves into another application).
