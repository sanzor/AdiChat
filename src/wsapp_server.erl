-module(wsapp_server).
-behaviour(gen_server).
-include("../include/domain.hrl").
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-export([
         start_link/0,
         create_user/1,
         delete_user/1,
         get_user/1,
         get_user_by_email/1,
         create_topic/1,
         delete_topic/1,
         publish/1,
         online/2,
         offline/2,
         subscribe/2,
         unsubscribe/2,
         get_older_messages/3,
         get_newest_messages/2,
         get_subscriptions/1]).

-define(SERVER,?MODULE).
-define(CONSTANT,<<"_events">>).
-define(F(UserId),<<(integer_to_binary(UserId))/binary,?CONSTANT/binary>>).
-record(state,{

}).
%----------------------------API ----------------------------------%

-spec get_user(UserId::domain:user_id())->{ok,User::domain:user()} | {error,Error::any()} | user_does_not_exist.

get_user(UserId)->
    gen_server:call(?MODULE,{get_user,UserId}).

-spec get_user_by_email(Email::binary())->{ok,User::domain:user()} |  user_does_not_exist.

get_user_by_email(Email)->
    gen_server:call(?MODULE,{get_user_by_email,Email}).

-spec create_user(UserData::domain:create_user_params())->{ok,User::domain:user()} 
                        | {error,{400,ValidationErrors::list()}}
                        | {error,Error::any()}.

create_user(UserData)->
    io:format("\nInside create user"),
    gen_server:call(?MODULE,{create_user,UserData}).


-spec delete_user(UserId::domain:user_id())->ok | {error,Error::any()}.
delete_user(UserId)->
    gen_server:call(?MODULE,{delete_user,UserId}).


-spec create_topic(TopicData::domain:create_topic_params())->
                        {ok,Topic::domain:topic()} 
                        | {error,{400,ValidationErrors::list()}}
                        | {error,Error::any()}.

create_topic(TopicData)->
    gen_server:call(?MODULE,{create_user,TopicData}).


-spec delete_topic(TopicId::domain:topic_id())->ok | {error,Error::any()}.
delete_topic(TopicId)->
    gen_server:call(?MODULE,{delete_user,TopicId}).





-spec get_older_messages(TopidId::domain:topic_id(),StartIndex::integer(),Count::integer())->{ok,Messages::[domain:message()]} | error .
get_older_messages(TopicId,StartIndex,Count)->
    gen_server:call(?MODULE,{get_messages,{TopicId,StartIndex,Count}}).


-spec get_newest_messages(TopidId::domain:topic_id(),Count::integer())->{ok,Messages::[domain:message()]} | error .
get_newest_messages(TopicId,Count)->
    gen_server:call(?MODULE,{get_messages,{TopicId,Count}}).

-spec get_subscriptions(UserId::domain:user_id())->{ok,Channels::[domain:user_topic()]}  | {error,Reason::any()}.
get_subscriptions(UserId)->
    gen_server:call(?MODULE,{get_subscriptions,UserId}).

-spec publish(Message::domain:message())->ok.
publish(Message)->
    gen_server:cast(?MODULE, {publish,Message}).


-spec online(UserId::domain:user_id(),Socket::pid())->ok.
online(UserId,Socket)->
    gen_server:call(?MODULE, {online,{UserId,Socket}}).


-spec offline(UserId::domain:user_id() |iodata(),Socket::pid())->ok.
offline(UserId,Socket)->
    gen_server:call(?MODULE, {offline,{UserId,Socket}}).

-spec subscribe(UserId::domain:user_id(),TopicId::domain:topic_id() )->{ok,SubscribeResult::domain:subscribe_result()}| already_subscribed | {error,Error::term()}.
subscribe(UserId,TopicName)->
    gen_server:call(?MODULE, {subscribe,{UserId,TopicName}}).


-spec unsubscribe(UserId::domain:user_id(),TopicId::domain:topic_id())->{ok,{unsubsribed,TopicId::domain:topic_id()}}| not_joined | {error,Error::term()}.
unsubscribe(UserId,TopicId)->
    gen_server:call(?MODULE, {unsubscribe,{UserId, TopicId}}).


start_link()->
    gen_server:start_link({local,?SERVER}, ?MODULE, [], []).



%-------------callbacks---------------------------------------%

init(Args)->
    process_flag(trap_exit,true),
    self() ! start,
    {ok,#state{}}.

%% @doc 
%% Handling call messages
%% @end

handle_call({get_user,UserId},_,State)->
    case storage:get_user(UserId) of
        {ok,User} ->{reply,{ok,User},State};
        user_does_not_exist ->{reply,user_does_not_exist,State}
    end;

handle_call({get_user_by_email,Email},_,State)->
    case storage:get_user_by_email(Email) of
        {ok,User} ->
           
            {reply,{ok,User},State};
         user_does_not_exist ->{reply,user_does_not_exist,State}
    end;
handle_call({create_user,UserData},_,State)->
   
    case validator:validate_user_data(UserData) of
        {false,ValidationErrors} -> {error,{400,ValidationErrors}};
        
         true -> io:format("\n:~p\n",[UserData]),
                case storage:create_user(UserData) of
                     already_exists ->{reply,{error,user_already_exists},State};
                    {ok,User} -> io:format("\nResulting User:~p",[User]),
                                 {reply,{ok,User},State};
                    {error,Error}->{reply,{error,Error},State};
                     _->{reply,already_exists,State}
                 end
    end;

handle_call({delete_user,UserId},_,State)->
    case storage:delete_user(UserId) of
         ok -> {reply,ok,State};
        {error,Error}->{reply,{error,Error},State}
        
    end;

handle_call({create_topic,TopicData},_,State)->
    case validator:validate_topic_data(TopicData) of
        {false,ValidationErrors} -> {error,{400,ValidationErrors}};
         true -> case storage:create_topic(TopicData) of
                    {ok,Topic} -> {reply,{ok,Topic},State};
                    {error,Error}->{reply,{error,Error},State}
                 end
    end;

handle_call({delete_topic,TopicId},_,State)->
    case storage:delete_user(TopicId) of
         ok -> {reply,ok,State};
        {error,Error}->{reply,{error,Error},State}
            
    end;
handle_call({get_older_messages,{TopicId,StartIndex,Count}},_,State)->
    {ok,Messages}=storage:get_oldest_messages(TopicId, StartIndex, Count),
    {reply,{ok,Messages},State};

handle_call({get_newest_messages,{TopicId,Count}},_,State)->
    {ok,Messages}=storage:get_newest_messages(TopicId, Count),
    {reply,{ok,Messages},State};

handle_call({get_subscriptions,UserId},_,State)->
    {ok,Subscriptions}=storage:get_user_subscriptions(UserId),
    {reply,{ok,Subscriptions},State};
 

handle_call({subscribe,{UserId,TopicName}},{From,_},_State)->
    Topic=#topic{id= TopicId}= case storage:get_topic_by_name(TopicName) of
                                        topic_does_not_exist -> 
                                             Params=#create_topic_params{user_id = UserId , name = TopicName},
                                            {ok,Topic}=storage:create_topic(Params),
                                            Topic;
                                        {ok,Topic} -> Topic
                                 end,
    Reply=case storage:check_if_subscribed(TopicId, UserId) of
        {ok,true} -> already_subscribed;
        {ok,false} ->ok=storage:subscribe(TopicId, UserId),
                UserEvent=#{user_event_kind => <<"subscribe">>,topic => Topic},
                [send(Socket,{user_event,UserId,UserEvent})|| 
                            Socket<-pg:get_members(?F(UserId)), Socket =/=From],
                Result=#subscribe_result{result = Topic,subscriber_id = UserId },
                {ok,Result}
    end,
   
    io:format("\n Subscribe result: ~p\n",[Reply]),
    {reply,Reply,_State};
   

handle_call({unsubscribe,{UserId,TopicId}},{From,_},State)->
    Reply=case storage:unsubscribe(TopicId, UserId) of 
                ok ->   
                        UserEvent=#{user_event_kind => <<"unsubscribe">>,topicId => TopicId},
                        [send(Socket,{user_event,UserId,UserEvent})
                            || Socket<-pg:get_members(?F(UserId)), Socket =/= From],
                        {ok,{unsubscribed,TopicId}};
                not_joined->not_joined
          end,
    {reply,Reply,State};      


handle_call({online,{UserId,Socket}},_,State)->
    UserEventGroup= ?F(UserId),
    ok=pg:join(UserId,Socket),
    ok=pg:join(UserEventGroup, Socket),
    {reply,ok,State};
    
handle_call({offline,{UserId,Socket}},_,State)->
    UserEventGroup=?F(UserId),
    ok=pg:leave(UserId, Socket),
    ok=pg:leave(UserEventGroup,Socket),
    {reply,ok,State}.






%% @doc 
%% 
%% Handling cast messages
%% @end
handle_cast({publish,Message=#message{}},State)->
    ok=storage:write_chat_message(Message),
    {ok,Subscribers}=storage:get_subscriptions_for_topic(Message#message.topic_id),
    io:format("\nWill send message:~p to subscribers:~p\n",[Message,Subscribers]),
    io:format("\nSubscribers to topic ~p : ~p\n", [Message#message.topic_id,Subscribers]),
    UIDS=lists:map(fun(_=#{<<"user_id">>:=UID})->UID end, Subscribers),
    [[send(Socket,Message)|| Socket<-online_sockets(Subscriber)] || Subscriber<-UIDS],
    {noreply,State}.

%% @doc 
%% Handling info messages
%% @end
handle_info(start,State)->
    ets:new(subscribers,[named_table,bag]),
    {noreply,State};

handle_info(Message,State)->
    {reply,Message,State}.

terminate(_Reason,_State)->ok.
send(Socket,Message)->
    io:format("Sending to session ~p",[Socket]),
    Socket ! Message.
online_sockets(User)->
    Sockets=pg:get_members(User),
    io:format("Sockets : ~p",[Sockets]),
    Sockets.



