-module(wsapp_ws).
-export([init/1,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         terminate/3]).







init(#{req :=Req})->
    #{bindings :=UserMap}=Req,
    {ok,UserMap}.

websocket_init(State)->
    #{<<"user">> :=User}=State,
    ok=wsapp_server: