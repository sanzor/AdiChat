-module(wsapp_server).
-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-export([publish/2,
         online/2,
         offline/2,
         subscribe/2]).

publish(Topic,Message)->
