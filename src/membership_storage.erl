-module(membership_storage).
-export([subscribe/2,unsubscribe/2,get_subscriptions_for_topic/1]).


-spec subscribe(Topic::binary(),User::binary()) -> ok | already_subscribed | {error,Error::term()}.
subscribe(Topic,User)->
    Pg=application:get_env(wsapp,pg),
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
    Pg=application:get_env(wsapp,pg),
    Hostname=proplists:get_value(hostname,Pg),
    Port=proplists:get_value(port,Pg),
    Username=proplists:get_value(username,Pg),
    Password=proplists:get_value(password,Pg),
    Database=proplists:get_value(database,Pg),
    Statement= <<"DELETE FROM  wschat_user WHERE user_id = $1 AND topic =$2">>,
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
-spec get_subscriptions_for_topic(Topic::binary())-> {ok,list()} | {error,Reason::term()}.
get_subscriptions_for_topic(Topic)->
    Pg=application:get_env(wsapp,pg),
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
    {ok,_}=epgsql:equery(C,Statement,[Topic]),
    ok.