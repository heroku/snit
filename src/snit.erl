%%%-------------------------------------------------------------------
%% @doc snit public interface
%% @end
%%%-------------------------------------------------------------------
-module(snit_app).
-export([start/4]).

-include("snit.hrl").

%% Assumes the snit application itself is started
-spec start(Name::atom(), Acceptors::pos_integer(), inet:port(), fun()) -> ok.
start(Name, Acceptors, ListenPort, SNIFun) ->
    SSLOpts = [
        {alpn_preferred_protocols, [?ALPN_HEROKU_TCP, ?ALPN_HTTP1]},
        {ciphers, element(2, application:get_env(snit, cipher_suites))},
        {honor_cipher_order, true},
        {port, ListenPort},
        {sni_fun, SNIFun}
        %% missing: reuse_session, reuse_sessions
    ],
    {ok, _} = ranch:start_listener(
        Name,
        Acceptors,
        ranch_ssl,              % transport
        SSLOpts,
        snit_echo,              % protocol
        []                      % protocol opts
    ).
