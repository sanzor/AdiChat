-module(storage).
-export([subscribe/2,
         unsubscribe/2,
         check_if_subscribed/2,
         get_subscriptions_for_topic/1,
         get_user_subscriptions/1,
         get_messages_for_topic/3,
         write_message_to_topic/2,
         write_messages_to_topic/2]).

-define(DB_SERVER_KEY,pg2).
-spec subscribe(Topic::binary(),User::binary()) -> ok  | {error,Error::term()}.
subscribe(Topic,User)->
    {ok,Pg}=application:get_env(wsapp,?DB_SERVER_KEY),
    Hostname=proplists:get_value(hostname,Pg),
    Port=proplists:get_value(port,Pg),
    Username=proplists:get_value(username,Pg),
    Password=proplists:get_value(password,Pg),
    Database=proplists:get_value(database,Pg),
    Statement= <<"INSERT INTO  wschat_user(topic,user_id) values ($1,$2)">>,
    {ok,C}=epgsql:connect(#{
        host=>Hostname,
        port=>Port,
        username=>Username,
        password=>Password,
        database=>Database,
        timeout=>4000
    }),
    {ok,_}=epgsql:equery(C,Statement,[Topic,User]),
     ok.                 
   


-spec unsubscribe(Topic::binary(),User::binary())-> ok | not_joined | {error,Error::term()}.
unsubscribe(Topic,User)->
    {ok,Pg}=application:get_env(wsapp,?DB_SERVER_KEY),
    Hostname=proplists:get_value(hostname,Pg),
    Port=proplists:get_value(port,Pg),
    Username=proplists:get_value(username,Pg),
    Password=proplists:get_value(password,Pg),
    Database=proplists:get_value(database,Pg),
    Statement= <<"DELETE FROM  wschat_user WHERE topic=$1 and user_id = $2">>,
    {ok,C}=epgsql:connect(#{
        host=>Hostname,
        port=>Port,
        username=>Username,
        password=>Password,
        database=>Database,
        timeout=>4000
    }),
    {ok,Rows}=epgsql:equery(C,Statement,[Topic,User]),
    io:format("\n Rows:~p",[Rows]),
    if Rows>0 -> ok ; true -> not_joined end.
    
-spec get_subscriptions_for_topic(Topic::binary())-> {ok,list()} | {error,Reason::term()}.
get_subscriptions_for_topic(Topic)->
    {ok,Pg}=application:get_env(wsapp,?DB_SERVER_KEY),
    Hostname=proplists:get_value(hostname,Pg),
    Port=proplists:get_value(port,Pg),
    Username=proplists:get_value(username,Pg),
    Password=proplists:get_value(password,Pg),
    Database=proplists:get_value(database,Pg),
    Statement= <<"SELECT user_id FROM  wschat_user WHERE topic = $1">>,
    {ok,C}=epgsql:connect(#{
        host=>Hostname,
        port=>Port,
        username=>Username,
        password=>Password,
        database=>Database,
        timeout=>4000
    }),
    {ok,_,Result}=epgsql:equery(C,Statement,[Topic]),
    {ok,normalize(Result)}.

-spec get_user_subscriptions(User::binary())-> {ok,Subscriptions::list()}  | {error,Error::term()}.
get_user_subscriptions(User)-> 
    {ok,Pg}=application:get_env(wsapp,?DB_SERVER_KEY),
    io:format("\n PgEnv:~p\n",[Pg]),
    Hostname=proplists:get_value(hostname,Pg),
    Port=proplists:get_value(port,Pg),
    Username=proplists:get_value(username,Pg),
    Password=proplists:get_value(password,Pg),
    Database=proplists:get_value(database,Pg),
    Statement= <<"SELECT topic FROM  wschat_user WHERE user_id = $1">>,
    {ok,C}=epgsql:connect(#{
        host=>Hostname,
        port=>Port,
        username=>Username,
        password=>Password,
        database=>Database,
        timeout=>4000
    }),
    {ok,_,Result}=epgsql:equery(C,Statement,[User]),
    io:format("\n query ok\n"),
    {ok,normalize(Result)}.
    
-spec check_if_subscribed(Topic::binary(),User::binary())->{ok,boolean()}|{error,Error::term()}.
check_if_subscribed(Topic,User)->
    {ok,Pg}=application:get_env(wsapp,?DB_SERVER_KEY),
    Hostname=proplists:get_value(hostname,Pg),
    Port=proplists:get_value(port,Pg),
    Username=proplists:get_value(username,Pg),
    Password=proplists:get_value(password,Pg),
    Database=proplists:get_value(database,Pg),
    Statement= <<"SELECT Count(user_id) FROM  wschat_user WHERE topic = $1 AND user_id=$2">>,
    {ok,C}=epgsql:connect(#{
        host=>Hostname,
        port=>Port,
        username=>Username,
        password=>Password,
        database=>Database,
        timeout=>4000
    }),
    {ok,_,[{Count}]}=epgsql:equery(C,Statement,[Topic,User]),
    {ok,Count>0}.


-spec get_messages_for_topic(Topic::binary(),StartIndex::binary(),Count::integer())->{ok,Messages::list()}| {error,Error::term()}.

get_messages_for_topic(Topic,StartIndex,Count)->
    {ok,Pg}=application:get_env(wsapp,?DB_SERVER_KEY),
    io:format("\n PgEnv:~p\n",[Pg]),
    Hostname=proplists:get_value(hostname,Pg),
    Port=proplists:get_value(port,Pg),
    Username=proplists:get_value(username,Pg),
    Password=proplists:get_value(password,Pg),
    Database=proplists:get_value(database,Pg),
    Statement= <<"SELECT * FROM messages WHERE topic = $1 AND index >= $2 ORDER BY index ASC LIMIT $3">>,
    {ok,C}=epgsql:connect(#{
        host=>Hostname,
        port=>Port,
        username=>Username,
        password=>Password,
        database=>Database,
        timeout=>4000
    }),
    {ok,_,Result}=epgsql:equery(C,Statement,[Topic,StartIndex,Count]),
    io:format("\n query ok\n"),
    {ok,normalize(Result)}.

-spec write_message_to_topic(Topic::binary(),Message::any())->ok | {error,Error::any()}.
write_message_to_topic(Topic,_Message=#{ topic:=Topic,user_id :=UserId,content:=Content,createdAt:=CreatedAt})->
    {ok,Pg}=application:get_env(wsapp,?DB_SERVER_KEY),
    Hostname=proplists:get_value(hostname,Pg),
    Port=proplists:get_value(port,Pg),
    Username=proplists:get_value(username,Pg),
    Password=proplists:get_value(password,Pg),
    Database=proplists:get_value(database,Pg),
    Statement= <<"INSERT INTO  messages(topic,user_id,content,createdAt) values ($1,$2,$3,$4)">>,
    {ok,C}=epgsql:connect(#{
        host=>Hostname,
        port=>Port,
        username=>Username,
        password=>Password,
        database=>Database,
        timeout=>4000
    }),
    {ok,_}=epgsql:equery(C,Statement,[Topic,UserId,Content,CreatedAt]),
     ok.   

-spec write_messages_to_topic(Topic::binary(),Messages::list())->{ok,Inserted::integer()} | {error,Error::any()}.
write_messages_to_topic(Topic,Messages)->
    {ok,Pg}=application:get_env(wsapp,?DB_SERVER_KEY),
    Hostname=proplists:get_value(hostname,Pg),
    Port=proplists:get_value(port,Pg),
    Username=proplists:get_value(username,Pg),
    Password=proplists:get_value(password,Pg),
    Database=proplists:get_value(database,Pg),
    Statement= <<"INSERT INTO  messages(topic,user_id,content,createdAt) values ($1,$2,$3,$4)">>,
    {ok,C}=epgsql:connect(#{
        host=>Hostname,
        port=>Port,
        username=>Username,
        password=>Password,
        database=>Database,
        timeout=>4000
    }),
    {ok,_}=epgsql:equery(C,Statement,[Topic,UserId,Content,CreatedAt]),
     ok.   
normalize(List)->
    normalize(List,[]).
-spec normalize(List::list(),Acc::list())->list().
normalize([],Acc) ->
    Acc;
normalize([{Head}|Tail], Acc) when is_list([Head|Tail])->
    normalize(Tail,[Head|Acc]).
