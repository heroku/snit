%%%-------------------------------------------------------------------
%% @doc snit public interface
%% @end
%%%-------------------------------------------------------------------
-module(snit).
-export([start/6, stop/1]).

-include("snit.hrl").

%% Assumes the snit application itself is started
%% Should pick a proper proxy default to be usable, but we need to be
%% able to override if only to support proxy protocol at some point.
%% We could just package all the required modules for easy use.
-spec start(Name::atom(), Acceptors::pos_integer(), inet:port(), fun(), module(), term()) -> ok.
start(Name, Acceptors, ListenPort, SNIFun, Protocol, ProtoOpts) ->
    Ciphers = [Cipher ||
                  {_Name, Cipher} <-  element(2, application:get_env(snit,
                                                                     cipher_suites))],
    SSLOpts = [
               {alpn_preferred_protocols, [?ALPN_HEROKU_TCP, ?ALPN_HTTP1]},
               {ciphers, Ciphers},
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
                Protocol,
                ProtoOpts
               ).

stop(Name) ->
    ranch:stop_listener(Name).
