-module(wsapp_server).
-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-export([
         start_link/0,
         publish/2,
         online/2,
         offline/2,
         subscribe/2,
         unsubscribe/2,
         get_messages/1,
         get_subscriptions/1]).

-define(SERVER,?MODULE).
-define(CONSTANT,<<"_events">>).
-define(F(User),<<User/binary,?CONSTANT/binary>>).
-record(state,{

}).
%----------------------------API ----------------------------------%
-spec get_messages(Topic::binary())->{ok,Messages::list()} | error .
get_messages(Topic)->
    gen_server:call(?MODULE,{get_messages,Topic}).

-spec get_subscriptions(User::string())->{ok,Channels::list()}  | {error,Reason::any()}.
get_subscriptions(User)->
    gen_server:call(?MODULE,{get_subscriptions,User}).

-spec publish(Topic::string(),Message::any())->ok.
publish(Topic,Message)->
    gen_server:cast(?MODULE, {publish,{Topic,Message}}).


-spec online(User::string(),Socket::pid())->ok.
online(User,Socket)->
    gen_server:call(?MODULE, {online,{User,Socket}}).



-spec offline(User::string() |iodata(),Socket::pid())->ok.
offline(User,Socket)->
    gen_server:call(?MODULE, {offline,{User,Socket}}).

-spec subscribe(User::string() | iodata(),Topic::string() |iodata())->ok.
subscribe(User,Topic)->
    gen_server:call(?MODULE, {subscribe,{User,Topic}}).


-spec unsubscribe(User::string(),Topic::string())->ok.
unsubscribe(User,Topic)->
    gen_server:call(?MODULE, {unsubscribe,{User, Topic}}).


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

handle_call({get_messages,Topic},_,State)->
    Messages=ets:match(messages, {Topic,'$1'}),
    {reply,{ok,Messages},State};

handle_call({get_subscriptions,User},_,State)->
    case get_subs(User) of
        no_subscriptions->{reply,no_subscriptions,State};
        Elements -> {reply,{ok,Elements},State}
    end;


handle_call({subscribe,{User,Topic}},{From,_},State)->
    Reply=case lists:any(fun(TargetUser)->TargetUser=:=User end,pg:get_members(Topic)) of
        true ->  #{result=> <<"already_subscribed">>};
        false -> true=ets:insert(subscribers, {Topic,User}),
                 Subscriptions=get_subs(User),
                 UserEvent=#{user_event_kind => <<"subscribe">>,topic => Topic, subscriptions=>Subscriptions},
                 [send(Socket,{user_event,User,UserEvent})|| Socket<-pg:get_members(?F(User)), Socket =/=From],
                 #{result=> <<"ok">> ,subscriptions=>Subscriptions}
                 
        end,
    {reply,Reply,State};


handle_call({unsubscribe,{User,Topic}},{From,_},State)->
    Reply=case ets:match_object(subscribers, {Topic,User}) of
        [{Topic,User}] ->
            true=ets:delete_object(subscribers,{Topic,User}),
            Subscriptions=get_subs(User),
            UserEvent=#{user_event_kind => <<"unsubscribe">>,topic => Topic, subscriptions=>Subscriptions},
            io:format("\nFrom:~p, Existing:~p\n",[From,pg:get_members(?F(User))]),
            [send(Socket,{user_event,User,UserEvent})|| Socket<-pg:get_members(?F(User)), Socket =/= From],
            #{result=> <<"ok">> ,subscriptions=>Subscriptions};
        [] ->#{result=> <<"not joined">>}
            
    end,
    {reply,Reply,State};


handle_call({online,{User,Socket}},_,State)->
    UserEventGroup= ?F(User),
    ok=pg:join(User,Socket),
    ok=pg:join(UserEventGroup, Socket),
    {reply,ok,State};
    
handle_call({offline,{User,Socket}},_,State)->
    UserEventGroup=?F(User),
    ok=pg:leave(User, Socket),
    ok=pg:leave(UserEventGroup,Socket),
    {reply,ok,State}.



%% @doc 
%% 
%% Handling cast messages
%% @end
handle_cast({publish,{Topic,Message}},State)->
    true=ets:insert(messages, {Topic,Message}),
    Subscribers=lists:concat(ets:match(subscribers,{Topic,'$1'})),
    io:format("\nSubscribers to topic ~p : ~p\n", [Topic,Subscribers]),
    [[send(Socket,Message)|| Socket<-online_sockets(Subscriber)] || Subscriber<-Subscribers ],
    {noreply,State}.


%% @doc 
%% Handling info messages
%% @end
handle_info(start,State)->
    ets:new(subscribers,[named_table,bag]),
    ets:new(messages,[named_table,bag]),
    {noreply,State};

handle_info(Message,State)->
    {reply,Message,State}.

terminate(_Reason,_State)->ok.
send(Socket,Message)->
    Socket ! Message.
online_sockets(User)->
    Sockets=pg:get_members(User),
    Sockets.


get_subs(User)->
    lists:concat(ets:match(subscribers,{'$1',User})).

