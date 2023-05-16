%%%-------------------------------------------------------------------
%% @doc nova_admin public API
%% @end
%%%-------------------------------------------------------------------

-module(wsapp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok,Hosts}=application:get_env(wsapp, hosts),
    io:format("\nFound Hosts: ~p\n",[Hosts]),
    io:format("\nResults from pinging~p",[{Host,ping_node(Host)}||Host<-Hosts]),
    wsapp_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

ping_node(Node)->
    try 
      pong=net_adm:ping(Node)
    catch
      Error:Reason -> io:format("\nError:~p, Reason~p\n",[Error,Reason]),
                      timer:sleep(20000),
                      net_adm:ping(Node)
    end.
%%====================================================================
%% Internal functions
%%====================================================================
