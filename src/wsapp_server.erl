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

handle_call({subscribe,{User,Topic}},_,State)->
    true=ets:insert(subscribers, {Topic,User}),
    Subs=get_subs(User),
    {reply,{ok,Subs},State};
handle_call({unsubscribe,{User,Topic}},_,State)->
    case ets:match_object(subscribers, {Topic,User}) of
        [{Topic,User}] -> 
            io:format("deleting"),
            true=ets:delete_object(subscribers,{Topic,User}),
            Subs=get_subs(User),
            io:format("Remaining subs :~p",[Subs]),
            {reply,{ok,Subs},State};
        [] ->
            logger:info("User :~p not subscribed to topic:~p~n",[User,Topic]),
            Subs=get_subs(User),
            {reply,{ok,Subs},State}
    end;
handle_call({online,{User,Socket}},_,State)->
    true=ets:insert(online,{User,Socket}),
    {reply,ok,State};
handle_call({offline,{User,Socket}},_,State)->
    ets:delete_object(online,{User,Socket}),
    {reply,ok,State}.



%% @doc 
%% 
%% Handling cast messages
%% @end
handle_cast({publish,{Topic,Message}},State)->
    true=ets:insert(messages, {Topic,Message}),
    Subscribers=ets:match(subscribers,{Topic,'$1'}),
    [[send(Socket,Message)|| Socket<-online_sockets(Subscriber)] || Subscriber<-Subscribers ],
    {noreply,State}.


%% @doc 
%% Handling info messages
%% @end
handle_info(start,State)->
    ets:new(subscribers,[named_table,bag]),
    ets:new(online,[named_table,bag]),
    ets:new(messages,[named_table,bag]),
    {noreply,State};

handle_info(Message,State)->
    {reply,Message,State}.

terminate(_Reason,_State)->ok.
send(Socket,Message)->
    Socket ! Message.
online_sockets(User)->
    Sockets=ets:match(online, {User,'$1'}),
    io:format("\nInside online sockets sending...\n"),
    io:format("\nSockets: ~p\n",[Sockets]),
    Sockets.


get_subs(User)->
    lists:concat(ets:match(subscribers,{'$1',User})).