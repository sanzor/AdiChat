-module(wsapp_ws).
-export([init/1,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         terminate/3]).

-define(HEARTBEAT,1500).
init(#{req :=Req})->
    #{bindings := #{<<"id">> := Id}}=Req,
    io:format("\nSetting up session with UserId:~p\n",[binary_to_integer(Id)]),
    {ok,#{<<"id">> => binary_to_integer(Id)}}.

websocket_init(State=#{<<"id">> :=Id})->
     
    {ok,User}= wsapp_server:get_user(Id),                         
    io:format("User is:~p",[User]),      
    #{<<"id">> :=UserId}=User,
    ok=wsapp_server:online(UserId,self()),
    {reply,ping,State#{<<"user">>=>User}}.

websocket_info(send_ping,State)->
    {reply,ping,State};


websocket_info({user_event,User,UserEventMessage}, State=#{<<"user">> :=User})->
    Reply=UserEventMessage#{kind=><<"user_event">>,user=>User},
    {reply,{text,thoas:encode(Reply)},State};

    
websocket_info(Message,State)->
    io:format("\nWeird: ~p\ntrace",[Message]),
    Reply=Message#{ kind=><<"chat">>},
    {reply,{text,thoas:encode(Reply)},State}.

websocket_handle({text, Message},State)->
    io:format("\nInput :~p\n",[Message]),
    Json=json:decode(Message,[maps]),
    io:format("\nReceived :~p\n",[Json]),
    #{<<"command">>:= Command}=Json,
    case handle_command(Command,Json,State) of
            {ok,noreply} -> {ok,State};
            {ok,reply,Reply} ->
                io:format("\n~p\n",[Reply]),
                {reply,{text,thoas:encode(Reply)},State}       
    end;

websocket_handle(pong, State)->
    erlang:send_after(?HEARTBEAT, self(), send_ping),
    {ok,State}.
terminate(_,_,State)->
    #{<<"id">> := Id}=State,
    wsapp_server:offline(Id,self()),
    ok.

handle_command(<<"create-topic">>,TopicData,_State)->
    BaseReply=#{kind=>"command_result",command=> <<"create-topic">>},
    Reply=case wsapp_server:create_topic(TopicData) of
        {ok,Topic}->BaseReply#{result=>Topic};
        {error,Error}->{error,Error}
    end,
    {ok,reply,Reply};

handle_command(<<"get-user">>,TopicData,_State)->
    BaseReply=#{kind=>"command_result",command=> <<"create-topic">>},
    Reply=case wsapp_server:create_topic(TopicData) of
        {ok,Topic}->BaseReply#{result=>Topic};
        {error,Error}->{error,Error}
    end,
    {ok,reply,Reply};

handle_command(<<"create-user">>,UserData,_State)->
    BaseReply=#{kind=>"command_result",command=> <<"create-user">>},
    Reply=case wsapp_server:create_user(UserData) of
        {ok,Topic}->BaseReply#{result=>Topic};
         already_exists->BaseReply#{result=>error,reason=>user_already_exists}
    end,
    {ok,reply,Reply};

handle_command(<<"subscribe">>,_=#{<<"topic">> :=Topic},_State=#{<<"id">> := UserId})->
    BaseReply=#{kind=><<"command_result">>, command=> <<"subscribe">>,  topic=>Topic},
    Reply=case wsapp_server:subscribe(UserId, Topic) of
        already_subscribed-> BaseReply#{result=><<"already_subscribed">>};
        {ok,Subscriptions} -> BaseReply#{result=><<"ok">>,subscriptions=>Subscriptions}
    end,    
    {ok,reply,Reply};
handle_command(<<"unsubscribe">>,_=#{<<"topicId">> :=TopicId},_State=#{<<"id">>:=UserId})->
    BaseReply=#{kind=><<"command_result">>, command=> <<"unsubscribe">>,  topic=>TopicId},
    Reply=case wsapp_server:unsubscribe(UserId,TopicId) of
        not_joined -> BaseReply#{result=><<"not_joined">>};
        {ok,Subscriptions} -> BaseReply#{result=><<"ok">>,subscriptions=>Subscriptions}
    end,
   
    {ok,reply,Reply};
  
handle_command(<<"publish">>,Json,_State)->
    io:format("Command publish: ~p",[Json]),
    #{<<"topicId">> := TopicId, <<"content">> := Content}=Json,
    #{<<"id">>:= UserId}=_State,
    
    DateTime=calendar:universal_time(),
    Message=#{
         <<"user_id">>=>UserId,
         <<"topic_id">>=>TopicId,
         <<"content">> => Content,
         <<"created_at">> =>DateTime
        },
   
    ok=wsapp_server:publish(TopicId,Message),
    {ok,noreply};

handle_command(<<"get_older_messages">>,Req=#{<<"topicId">> := TopicId, <<"startIndex">> :=StartIndex , <<"count">> := Count},_State)->
    io:format("~p",[Req]),
    {ok,Messages}=wsapp_server:get_oldest_messages(TopicId,StartIndex,Count),
    {ok,reply,#{<<"topic">>=>TopicId, <<"result">>=>Messages, kind=><<"command_result">>}};

handle_command(<<"get_newest_messages">>,Req=#{<<"topicId">> := TopicId, <<"count">> := Count},_State)->
    io:format("~p",[Req]),
    {ok,Messages}=wsapp_server:get_newest_messages(TopicId,Count),
    {ok,reply,#{<<"topic">>=>TopicId, <<"result">>=>Messages, kind=><<"command_result">>}};
    
handle_command(<<"get_subscriptions">>,_,_State=#{<<"id">>:=UserId})->
    Reply=#{command=> <<"get_subscriptions">>,kind=><<"command_result">>},
    case wsapp_server:get_subscriptions(UserId) of
        {ok,no_subscriptions}->{ok,reply,Reply#{result=> atom_to_binary(no_subscriptions)}};
        {ok,Result}->{ok,reply,Reply#{result =>Result}};  
        {error,Reason}->{error,Reason}
    end;
handle_command(<<"get_session_info">>, _,_State)->
    Reply=(system_data())#{command=><<"get_session_info">>,kind=><<"command_result">>},
    {ok,reply,Reply};


handle_command(_,_,_State)->
    {error,unknown_command}.



system_data()->
    BinPid=list_to_binary(pid_to_list(self())),
    NodePid=atom_to_binary(node()),
    #{node=>NodePid,pid=>BinPid}.

