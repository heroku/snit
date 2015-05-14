%%%-------------------------------------------------------------------
%% @doc snit application callback
%% @end
%%%-------------------------------------------------------------------
-module(snit_app).

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    check_ciphers(),
    snit_sup:start_link().

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
check_ciphers() ->
    {ok, Requirements} = application:get_env(snit, cipher_suites),
    Ciphers = [Suite || {_Name, Suite} <- Requirements],
    Supported = ssl:cipher_suites(erlang),
    case [Cipher || Cipher <- Ciphers, not lists:member(Cipher, Supported)] of
        [] ->
            ok;
        Unsupported ->
            exit({unsupported_ciphers, Unsupported})
    end.
