%%%-------------------------------------------------------------------
%% @doc snit public interface
%% @end
%%%-------------------------------------------------------------------
-module(snit).
-export([start/6, start/7, start_opts/6, stop/1, contains/2]).

-include("snit.hrl").

%% Assumes the snit application itself is started
%% Should pick a proper proxy default to be usable, but we need to be
%% able to override if only to support proxy protocol at some point.
%% We could just package all the required modules for easy use.
-spec start(Name::atom(), Acceptors::pos_integer(), inet:port(), fun(), module(), term()) ->
                   {ok, undefined | pid()}.
start(Name, Acceptors, ListenPort, SNIFun, Protocol, ProtoOpts) ->
    start(Name, Acceptors, ListenPort, SNIFun, Protocol, ProtoOpts, ranch_ssl).

start(Name, Acceptors, ListenPort, SNIFun, Protocol, ProtoOpts, SSLTransport) ->
    SSLOpts = [
               {port, ListenPort},
               {sni_fun, SNIFun}
              ],

    start_opts(Name, Acceptors, Protocol, ProtoOpts, SSLTransport, SSLOpts).

start_opts(Name, Acceptors, Protocol, ProtoOpts, SSLTransport, SSLOpts0) ->
    Ciphers = [Cipher ||
                  {_Name, Cipher} <-  element(2, application:get_env(snit,
                                                                     cipher_suites))],

    ALPN = application:get_env(snit, alpn_preferred_protocols, [?ALPN_HTTP1]),

    DefaultOps =
        [
         {alpn_preferred_protocols, ALPN},
         {ciphers, Ciphers},
         {honor_cipher_order, true},
         {secure_renegotiate, true},
         {client_renegotiation, false},
         {max_connections, infinity},
         {versions, ['tlsv1.2', 'tlsv1.1', 'tlsv1']}
         %% missing: reuse_session, reuse_sessions
        ],

    SSLOpts = orddict:merge(fun(_K, _A, B) ->
                                    case _A =/= B of
                                        true ->
                                            lager:warning("discarding new value ~p for key ~p as unsafe",
                                                          [_A, _K]);
                                        _ -> ok
                                    end,
                                    B
                            end, SSLOpts0, DefaultOps),

    case contains(port, SSLOpts) andalso
        contains(alpn_preferred_protocols, SSLOpts) andalso
        (contains(sni_fun, SSLOpts) orelse
         contains(sni_hosts, SSLOpts) orelse
         contains(cert, SSLOpts) orelse
         contains(certfile, SSLOpts)) of
        true ->
            {ok, _} = ranch:start_listener(
                        Name,
                        Acceptors,
                        SSLTransport,
                        SSLOpts,
                        Protocol,
                        ProtoOpts
                       );
        _ ->
            {error, missing_mandatory_configs, SSLOpts}
    end.

stop(Name) ->
    ranch:stop_listener(Name).

%%% internal functions

contains(Key, Dict) ->
    case proplists:get_value(Key, Dict) of
        undefined ->
            false;
        _ ->
            true
    end.
