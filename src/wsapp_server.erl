-module(wsapp_server).
-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-export([
         start_link/0,
         create_user/1,
         delete_user/1,
         create_topic/1,
         delete_topic/1,
         publish/2,
         online/2,
         offline/2,
         subscribe/2,
         unsubscribe/2,
         get_messages/3,
         get_subscriptions/1]).

-define(SERVER,?MODULE).
-define(CONSTANT,<<"_events">>).
-define(F(UserId),<<(integer_to_binary(UserId))/binary,?CONSTANT/binary>>).
-record(state,{

}).
%----------------------------API ----------------------------------%

-spec create_user(UserData::any())->{ok,User::any()} 
                        | {error,{400,ValidationErrors::list()}}
                        | {error,Error::any()}.

create_user(UserData)->
    gen_server:call(?MODULE,{create_user,UserData}).


-spec delete_user(UserId::number())->ok | {error,Error::any()}.
delete_user(UserId)->
    gen_server:call(?MODULE,{delete_user,UserId}).


-spec create_topic(TopicData::any())->{ok,Topic::any()} 
                        | {error,{400,ValidationErrors::list()}}
                        | {error,Error::any()}.

create_topic(TopicData)->
    gen_server:call(?MODULE,{create_user,TopicData}).


-spec delete_topic(TopicId::number())->ok | {error,Error::any()}.
delete_topic(TopicId)->
    gen_server:call(?MODULE,{delete_user,TopicId}).





-spec get_messages(TopidId::number(),StartIndex::integer(),Count::integer())->{ok,Messages::list()} | error .
get_messages(TopicId,StartIndex,Count)->
    gen_server:call(?MODULE,{get_messages,{TopicId,StartIndex,Count}}).

-spec get_subscriptions(UserId::integer())->{ok,Channels::list()}  | {error,Reason::any()}.
get_subscriptions(UserId)->
    gen_server:call(?MODULE,{get_subscriptions,UserId}).

-spec publish(TopicId::integer(),Message::any())->ok.
publish(TopicId,Message)->
    gen_server:cast(?MODULE, {publish,{TopicId,Message}}).


-spec online(UserId::integer(),Socket::pid())->ok.
online(UserId,Socket)->
    gen_server:call(?MODULE, {online,{UserId,Socket}}).


-spec offline(UserId::integer() |iodata(),Socket::pid())->ok.
offline(UserId,Socket)->
    gen_server:call(?MODULE, {offline,{UserId,Socket}}).

-spec subscribe(UserId::integer(),TopicId::integer() )->OkResult::map() | already_subscribed | {error,Error::term()}.
subscribe(UserId,TopicId)->
    gen_server:call(?MODULE, {subscribe,{UserId,TopicId}}).


-spec unsubscribe(UserId::integer(),TopicId::integer())->OkResult::map()| not_joined | {error,Error::term()}.
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

handle_call({create_user,UserData},_,State)->
    case validator:validate_user_data(UserData) of
        {false,ValidationErrors} -> {error,{400,ValidationErrors}};
         true -> case storage:create_user(UserData) of
                    {ok,User} -> {reply,{ok,User},State};
                    {error,Error}->{reply,{error,Error},State}
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
handle_call({get_messages,{TopicId,StartIndex,Count}},_,State)->
    {ok,Messages}=storage:get_messages(TopicId, StartIndex, Count),
    {reply,{ok,Messages},State};

handle_call({get_subscriptions,UserId},_,State)->
    {ok,Subscriptions}=storage:get_user_subscriptions(UserId),
    {reply,{ok,Subscriptions},State};
 


handle_call({subscribe,{UserId,TopicId}},{From,_},State)->
    Reply=case storage:check_if_subscribed(TopicId, UserId) of
        {ok,true} -> already_subscribed;
        {ok,false} ->ok=storage:subscribe(TopicId, UserId),
                {ok,Subscriptions}=storage:get_user_subscriptions(UserId),
                UserEvent=#{user_event_kind => <<"subscribe">>,
                topic => TopicId, subscriptions=>Subscriptions},
                [send(Socket,{user_event,UserId,UserEvent})|| 
                            Socket<-pg:get_members(?F(UserId)), Socket =/=From],
                {ok,Subscriptions}
    end,
    {reply,Reply,State};


handle_call({unsubscribe,{UserId,TopicId}},{From,_},State)->
    Reply=case storage:unsubscribe(TopicId, UserId) of 
                ok ->   {ok,Subscriptions}=storage:get_user_subscriptions(UserId),
                        UserEvent=#{user_event_kind => <<"unsubscribe">>,topic => TopicId, subscriptions=>Subscriptions},
                        [send(Socket,{user_event,UserId,UserEvent})
                            || Socket<-pg:get_members(?F(UserId)), Socket =/= From],
                        {ok,Subscriptions};
                not_joined->#{result=> <<"not joined">>}
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
handle_cast({publish,{TopicId,Message}},State)->
    ok=storage:write_chat_message(Message),
    {ok,Subscribers}=storage:get_subscriptions_for_topic(TopicId),
    io:format("\nWill send message:~p to subscribers:~p\n",[Message,Subscribers]),
    io:format("\nSubscribers to topic ~p : ~p\n", [TopicId,Subscribers]),
    [[send(Socket,Message)|| Socket<-online_sockets(Subscriber)] || Subscriber<-Subscribers ],
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
    Socket ! Message.
online_sockets(User)->
    Sockets=pg:get_members(User),
    Sockets.


