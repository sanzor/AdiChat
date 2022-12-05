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
         subscribe/2]).

-define(SERVER,?MODULE).

-record(state,{

}).
%----------------------------API ----------------------------------%

-spec publish(Topic::string(),Message::any())->ok.
publish(Topic,Message)->
    gen_server:cast(?SERVER, {publish,{Topic,Message}}).

online(User,Socket)->
    gen_server:call(?SERVER, {online,{User,Socket}}).

offline(User,Socket)->
    gen_server:call(?SERVER, {offline,{User,Socket}}).

subscribe(User,Topic)->
    gen_server:call(?SERVER, {subscribe,{User,Topic}}).

unsubscribe(User,Topic)->
    gen_server:call(?SERVER, {unsubscribe,{User, Topic}}).


start_link()->
    gen_server:start_link({local,?SERVER}, ?MODULE, [], []).



%-------------callbacks---------------------------------------%

init(Args)->
    self() ! start,
    {ok,#state{}}.

handle_call({subscribe,Topic,User},_,State)->
    ets:insert(subsribers, {Topic,User}),
    {reply,ok,State};
handle_call({unsubscribe,Topic,User},_,State)->
    case ets:match_object(subscribers, {Topic,User}) of
        [{Topic,User}] -> 
            ets:delete_object(subscribers,{Topic,User}),
            {reply,ok,State};
        [] -> {reply,ok,State}
    end;
handle_call({online,User,Socket},_,State)->
    ets:insert(online,{User,Socket}),
    {reply,ok,State};
handle_call({offline,User,Socket},_,State)->
    ets:delete_object(online,{User,Socket}),
    {reply,ok,State}.
handle_cast({publish,Topic,Message},State)->
    Subscribers=ets:match(subscribers,{Topic,'$1'}),
    [[send(S,Message)|| S<-online_sockets(Subscriber)] || Subscriber<-Subscribers ],
    {noreply,State}.


send(Socket,Message)->
    Socket ! Message.
online_sockets(User)->
    Sockets=ets:match(online, {User,'$1'}),
    Sockets.
