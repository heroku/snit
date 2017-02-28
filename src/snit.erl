%%%-------------------------------------------------------------------
%%% Copyright (c) 2015-2017, Heroku Inc <routing-feedback@heroku.com>,
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are
%%% met:
%%% 
%%% * Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer.
%%% 
%%% * Redistributions in binary form must reproduce the above copyright
%%%   notice, this list of conditions and the following disclaimer in the
%%%   documentation and/or other materials provided with the distribution.
%%% 
%%% * The names of its contributors may not be used to endorse or promote
%%%   products derived from this software without specific prior written
%%%   permission.
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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
    %% We know there's gonna be one valid cipher in each of the sublists because
    %% of snit_app.erl, which does a boot check. The options we get here (`CipherList')
    %% may contain more than one definition of each cipher for compatibility with
    %% Erlang 18.0-18.2 and 18.3+; we just have to find out which.
    Supported = ssl:cipher_suites(erlang),
    Ciphers = [Cipher ||
                  {_Name, CipherList} <- element(2, application:get_env(snit, cipher_suites)),
                  Cipher <- CipherList,
                  lists:member(Cipher, Supported)
              ],
    ECCs = case ssl_eccs0_supported() of
               true ->
                   case application:get_env(snit, eccs) of
                       undefined ->
                           [];
                       {ok, ConfECCs} ->
                           [{eccs, ConfECCs},
                            {honor_ecc_order, true}]
                   end;
               false -> % ssl < 19.2
                   []
           end,

    ALPN = application:get_env(snit, alpn_preferred_protocols, [?ALPN_HTTP1]),

    DefaultOpts =
        [
         {alpn_preferred_protocols, ALPN}
        ],

    OverrideOpts =
        [
         %% erlang currently does not support compression.
         %% If it ever does, disable it here to prevent CRIME and BREACH
         %% attacks.
         {ciphers, Ciphers},
         {honor_cipher_order, true},
         {secure_renegotiate, true},
         {client_renegotiation, false},
         {max_connections, infinity},
         {versions, ['tlsv1.2', 'tlsv1.1', 'tlsv1']}
         %% missing: reuse_session, reuse_sessions
        ] ++ ECCs,

    SSLOpts = lists:foldl(fun merge_opts/2,  DefaultOpts, [SSLOpts0, OverrideOpts]),

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

merge_opts(Opts1, Opts2) ->
    MergeFun = fun(K, A, B) ->
                          case A =/= B of
                              true ->
                                  error_logger:warning_msg("snit discarding value ~p for key ~p", [B, K]);
                              _ -> ok
                          end,
                          A
                  end,
    OrdOpts1 = orddict:from_list(Opts1),
    OrdOpts2 = orddict:from_list(Opts2),
    orddict:merge(MergeFun, OrdOpts1, OrdOpts2).

contains(Key, Dict) ->
    case proplists:get_value(Key, Dict) of
        undefined ->
            false;
        _ ->
            true
    end.

%% check if ECCs can be configured
-dialyzer({nowarn_function, ssl_eccs0_supported/0}).
ssl_eccs0_supported() ->
    try
        _ = ssl:eccs(),
        true
    catch
        error:undef ->
            false
    end.
