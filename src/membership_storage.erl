-module(membership_storage).
-export([subscribe/2,unsubscribe/2,get_subscriptions_for_topic/1]).


-spec subscribe(Topic::binary(),User::binary()) -> ok | already_subscribed | {error,Error::term()}.
subscribe(Topic,User)->
    Statement= <<"INSERT INTO table wschat_user(topic,user_id) values($1,$2)">>,
    {ok,C}=epgsql:connect(#{
        host=>"127.0.0.1",
        username=>"postgres",
        password=>"sanzor93",
        database=>"postgres_db",
        timeout=>4000
    }),
    {ok,_}=epgsql:execute(C,Statement,[Topic,User]),
    ok.

-spec unsubscribe(Topic::binary(),User::binary())-> ok | not_joined | {error,Error::term()}.
unsubscribe(Topic,User)->
    Statement= <<"DELETE FROM table wschat_user WHERE user_id = $1 AND topic =$2">>,
    {ok,C}=epgsql:connect(#{
        host=>"127.0.0.1",
        username=>"postgres",
        password=>"sanzor93",
        database=>"postgres_db",
        timeout=>4000
    }),
    {ok,_}=epgsql:execute(C,Statement,[Topic,User]),
    ok.
-spec get_subscriptions_for_topic(Topic::binary())-> {ok,list()} | {error,Reason::term()}.
get_subscriptions_for_topic(Topic)->
    Statement= <<"SELECT user_id FROM table wschat_user WHERE topic = $1">>,
    {ok,C}=epgsql:connect(#{
        host=>"127.0.0.1",
        username=>"postgres",
        password=>"sanzor93",
        database=>"postgres_db",
        timeout=>4000
    }),
    {ok,_}=epgsql:execute(C,Statement,[Topic]),
    ok.