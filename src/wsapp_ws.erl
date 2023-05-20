-module(wsapp_ws).
-export([init/1,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         terminate/3]).

-define(HEARTBEAT,1500).
init(#{req :=Req})->
    #{bindings :=UserMap}=Req,
    {ok,UserMap}.

websocket_init(State)->
    #{<<"user">> :=User, <<"cookie">> :=Cookie}=State,
    io:format("New User: ~p , Cookie:~p~n",[User,Cookie]),
    ok=wsapp_server:online(User,self()),
    {reply,ping,State}.

websocket_info(send_ping,State)->
    {reply,ping,State};

websocket_info({user_event,User,UserEventMessage}, State=#{<<"user">> :=User})->
    Reply=UserEventMessage#{kind=><<"user_event">>,user=>User},
    {reply,{text,thoase:encode(Reply)},State};
websocket_info(Message,State)->
    {ok,NewMessage}=thoas:decode(Message),
    Reply=NewMessage#{ kind=><<"chat">>},
    {reply,{text,thoas:encode(Reply)},State}.

websocket_handle({text, Message},State)->
    io:format("\nInput :~p\n",[Message]),
    Decode=json:decode(Message,[maps]),
    io:format("\nReceived :~p\n",[Decode]),
    #{<<"command">>:= Command}=Decode,
    case handle_command(Command,Decode,State) of
            {ok,noreply} -> {ok,State};
            {ok,reply,Reply} ->{reply,{text,thoas:encode(Reply)},State}
    end;

websocket_handle(pong, State)->
    erlang:send_after(?HEARTBEAT, self(), send_ping),
    {ok,State}.
terminate(_,_,State)->
    #{<<"user">> := User}=State,
    wsapp_server:offline(User,self()),
    ok.


handle_command(<<"subscribe">>,_=#{<<"topic">> :=Topic},_State=#{<<"user">> := User})->
    Result=wsapp_server:subscribe(User, Topic),
    Reply=#{ kind=><<"command_result">>, command=> <<"subscribe">>, result=> Result , topic=> Topic},
    {ok,reply,Reply};
handle_command(<<"unsubscribe">>,_=#{<<"topic">> :=Topic},_State=#{<<"user">>:=User})->
    Result=wsapp_server:unsubscribe(User,Topic),
    Reply=#{kind=><<"command_result">>, command=> <<"unsubscribe">>, result=> Result, topic=>Topic},
    {ok,reply,Reply};
  
handle_command(<<"publish">>,Decode,_State)->
    #{<<"topic">> := Topic}=Decode,
    #{<<"user">>:= User}=_State,
    Json=json:encode(Decode#{<<"user">>=>User},[maps,binary]),
    ok=wsapp_server:publish(Topic, Json),
    {ok,noreply};

handle_command(<<"get_messages">>,#{<<"topic">> := Topic},_State)->
    {ok,Messages}=wsapp_server:get_messages(Topic),
    {ok,reply,#{<<"topic">>=>Topic, <<"messages">>=>Messages, kind=><<"command_result">>}};

handle_command(<<"get_subscriptions">>,_,_State=#{<<"user">>:=User})->
    Reply=#{command=> <<"get_subscriptions">>,kind=><<"command_result">>},
    case wsapp_server:get_subscriptions(User) of
        {ok,no_subscriptions}->{ok,reply,Reply#{result=> atom_to_binary(no_subscriptions)}};
        {ok,Result}->{ok,reply,Reply#{result =>Result}};  
        {error,Reason}->{error,Reason}
end;
   
handle_command(_,_,_State)->
    {error,unknown_command}.
