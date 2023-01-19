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
    #{<<"user">> :=User, <<"cookie">> :=Cookie}=State,
    io:format("New User: ~p , Cookie:~p~n",[User,Cookie]),
    ok=wsapp_server:online(User,self()),
    {ok,State}.

websocket_info(Message,State)->
    {reply,Message,State}.

websocket_handle({text,Message},State)->
    io:format("Messagerz: ~p ~n", [Message]),
    Decode=json:decode(Message,[maps]),
    #{<<"user">> :=User}=State,
    #{<<"topic">> :=Topic}=Decode,
    %Json=json:encode(Decode#{<<"user">>=>User},[maps,binary]),
    ok=wsapp_server:publish(Topic,Message),
    {ok,State};

websocket_handle({text, Message=#{<<"command">> := Command}},State)->
    io:format("Received :~p",[Message]),
    try 
        case handle_command(Command,Message,State) of
            {ok,noreply} -> {ok,State};
            {ok,reply,Reply} ->{reply,Reply,State}
        end
    catch
        Error:Reason ->{reply,{error,Error,reason,Reason},State}
    end;
websocket_handle(pong,State)->
    io:format("~npong~n"),
    {reply,ping,State};

websocket_handle(ping,State)->
    io:format("ping"),
    {reply,pong,State};
websocket_handle(sugi,State)->
    {ok,State}.
terminate(_,_,State)->
    #{<<"user">> := User}=State,
    wsapp_server:offline(User,self()),
    ok.


  

handle_command(<<"get_messages">>,#{<<"topic">> := Topic},_State)->
    {ok,Messages}=wsapp_server:get_messages(Topic),
    {ok,reply,Messages};
handle_command(<<"get_subscriptions">>,_,_State=#{<<"user">>:=User})->
    {ok,Subscriptions}=wsapp_server:get_subscriptions(User),
    {ok,reply,Subscriptions}.