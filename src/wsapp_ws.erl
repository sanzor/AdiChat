-module(wsapp_ws).
-include("../include/domain.hrl").
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
    #user{id =UserId}=User,
    ok=wsapp_server:online(UserId,self()),
    {reply,ping,State#{<<"user">>=>User}}.

websocket_info(send_ping,State)->
    {reply,ping,State};


websocket_info({user_event,User,UserEventMessage}, State=#{<<"user">> :=User})->
    
    io:format("\nInside info: self: ~p\n",[self()]),
    io:format("\nSending from info:~p",[UserEventMessage]),
    Reply=UserEventMessage#{kind=><<"user_event">>,user=>User},
    {reply,{text,thoas:encode(Reply)},State};


websocket_info(_=#message{user_id = UserId,topic_id = TopicId,content = Content,created_at = CreatedAt,timezone = Timezone},State)->
    FormattedDateTime=utils:datetime_to_string(CreatedAt),
    Reply=#{user_id=>UserId,topic_id=>TopicId,content=>Content,created_at=>FormattedDateTime,timezone=>Timezone, kind=><<"chat">>},
    io:format("\nWeird: ~p\ntrace",[Reply]),
    {reply,{text,thoas:encode(Reply)},State}.

websocket_handle({text, Message},State)->
    io:format("\nInput :~p\n",[Message]),
    Json=json:decode(Message,[maps]),
    io:format("\nReceived :~p\n",[Json]),
    #{<<"command">>:= Command}=Json,
    case handle_command(Command,Json,State) of
            {ok,noreply} -> {ok,State};
            {ok,reply,Reply} ->
                io:format("\n Sending :~p\n",[Reply]),
                {reply,{text,thoas:encode(Reply)},State}       
    end;

websocket_handle(pong, State)->
    erlang:send_after(?HEARTBEAT, self(), send_ping),
    {ok,State}.
terminate(_,_,State)->
    #{<<"id">> := Id}=State,
    wsapp_server:offline(Id,self()),
    ok.

handle_command(<<"create-topic">>,_=#{<<"name">>:=Name,<<"user_id">>:=UserId},_State)->
    BaseReply=#{kind=>"command_result",command=> <<"create-topic">>},
    
    Reply=case wsapp_server:create_topic(#create_topic_params{name = Name,user_id = UserId}) of
        {ok,Topic}->BaseReply#{result=>utils:from_topic(Topic)};
        {error,Error}->{error,Error}
    end,
    {ok,reply,Reply};

handle_command(<<"get-user">>,UserId,_State)->
    BaseReply=#{kind=>"command_result",command=> <<"create-topic">>},
    Reply=case wsapp_server:get_user(UserId) of
        {ok,User}->BaseReply#{result=>utils:from_user(User)};
        {error,Error}->{error,Error}
    end,
    {ok,reply,Reply};

handle_command(<<"create-user">>,UserData,_State)->
    BaseReply=#{kind=>"command_result",command=> <<"create-user">>},
    Reply=case wsapp_server:create_user(UserData) of
        {ok,User}->BaseReply#{result=>utils:from_user(User)};
         already_exists->BaseReply#{result=>error,reason=>user_already_exists}
    end,
    {ok,reply,Reply};

handle_command(<<"subscribe">>,_=#{<<"topic">> :=TopicName},_State=#{<<"id">> := UserId})->
    BaseReply=#{kind=><<"command_result">>, command=> <<"subscribe">>},
    Reply=case wsapp_server:subscribe(UserId, TopicName) of
            already_subscribed-> BaseReply#{result=><<"already_subscribed">>};
            {ok,#subscribe_result{result = Result,subscriber_id = SubscriberId}} -> 
                BaseReply#{result=><<"ok">>, <<"userId">> => SubscriberId,<<"topic">>=>utils:from_topic(Result)}
          end, 
    {ok,reply,Reply};

handle_command(<<"unsubscribe">>,_=#{<<"topicId">> :=TopicId},_State=#{<<"id">>:=UserId})->
    BaseReply=#{kind=><<"command_result">>, command=> <<"unsubscribe">>},
    Reply=case wsapp_server:unsubscribe(UserId,TopicId) of
        not_joined -> BaseReply#{result=><<"not_joined">>};
        {ok,{unsubscribed,TopicId}} -> BaseReply#{result=><<"ok">>,<<"topicId">>=>TopicId}
    end,
   
    {ok,reply,Reply};
  
handle_command(<<"publish">>,Json,_State)->
    io:format("Command publish: ~p",[Json]),
    #{<<"topicId">> := TopicId, <<"content">> := Content}=Json,
    #{<<"id">>:= UserId}=_State,
    
    DateTime=calendar:universal_time(),
    Message=#message{
         user_id =UserId,
         topic_id= TopicId,
         content = Content,
         created_at = DateTime,
         timezone= <<"utc+2">>
        },
   
    ok=wsapp_server:publish(Message),
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

