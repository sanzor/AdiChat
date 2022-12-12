%%%-------------------------------------------------------------------
%% @doc wsapp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(wsapp_sup).

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
    ChildSpecs=[
    #{
        id => wsapp_server,
        start=>{wsapp_server,start_link,[]},
        restart=> permanent,
        shutdown =>5000,
        type=>worker,
        modules=>[wsapp_server]
     }
    ],
    Strategy={one_for_all,0,1},
    {ok, { Strategy, ChildSpecs} }.

%%====================================================================
%% Internal functions
%%====================================================================
