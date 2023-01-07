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
    ok=wsapp_server:online(User,self()),
    {ok,State}.

websocket_info(Message,State)->
    {reply,Message,State}.

websocket_handle({text,Message},State)->
    Decode=json:decode(Message,[maps]),
    #{<<"user">> :=User}=State,
    #{<<"topic">> :=Topic}=Decode,
    %Json=json:encode(Decode#{<<"user">>=>User},[maps,binary]),
    ok=wsapp_server:publish(Topic,Message),
    {ok,State};

websocket_handle(pong,State)->
    {ok,State};
websocket_handle(sugi,State)->
    {ok,State}.
terminate(_,_,State)->
    #{<<"user">> := User}=State,
    wsapp_server:offline(User,self()),
    ok.