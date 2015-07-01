%%%-------------------------------------------------------------------
%% @doc snit top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(snit_store_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Store =
        case application:get_env(snit, store_type, ets) of
            ets ->
                snit_ets_store;
            bitcask ->
                snit_bc_store
        end,
    {ok, {{one_for_all, 5, 10},
          [
            {store,
             {snit_cert_store, start_link, [Store, []]},
             permanent, 5000, worker, [snit_cert_store]}
          ]
         }}.

%%====================================================================
%% Internal functions
%%====================================================================
