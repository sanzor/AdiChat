-module(storage).
-export([
         to_map/3,
         create_user/1,
         get_user/1,
         get_user_by_email/1,
         delete_user/1,
         create_topic/1,
         delete_topic/1,
         does_topic_exist/1,
         subscribe/2,
         unsubscribe/2,
         check_if_subscribed/2,
         get_subscriptions_for_topic/1,
         get_user_subscriptions/1,
         get_messages/3,
         write_chat_message/1,
         write_chat_messages/1]).
-define(DB_SERVER_KEY,pg2).



create_connection()->
    {ok,Pg}=application:get_env(wsapp,?DB_SERVER_KEY),
    Hostname=proplists:get_value(hostname,Pg),
    Port=proplists:get_value(port,Pg),
    UserName=proplists:get_value(username,Pg),
    Password=proplists:get_value(password,Pg),
    Database=proplists:get_value(database,Pg),
    {ok,C}=epgsql:connect(#{
        host=>Hostname,
        port=>Port,
        username=>UserName,
        password=>Password,
        database=>Database,
        timeout=>4000
    }),
    {ok,C}.
    
-spec get_user(UserId::integer())->{ok,User::map()} | {error,Error::any()}.
get_user(UserId)->
    Statement= <<"Select * FROM wsuser WHERE id=$1">>,
    {ok,C}=create_connection(),
    {ok,[Result]}=epgsql:equery(C,Statement,[UserId]),
    {ok,Result}.

get_user_by_email(Email)->
    Statement= <<"Select * FROM wsuser WHERE email=$email">>,
    {ok,C}=create_connection(),
    {ok,[Result]}=epgsql:equery(C,Statement,[Email]),
    {ok,Result}.


-spec create_user(UserData::map())-> {ok,User::map()} | already_exists | {error,Error::any()}.
create_user(_UserData=#{<<"name">> :=UserName})->
   io:format("\nInside create_user storage\n:~p",[_UserData]),
    try
        Statement= <<"INSERT INTO  wsuser(name) values ($1) RETURNING *">>,
        {ok,C}=create_connection(),
        {ok,_,Columns,[Values]}=epgsql:equery(C,Statement,[UserName]),
        Value=to_map(Columns, tuple_to_list(Values),#{}),
        {ok,Value}
    catch
        Error:Reason -> {error,{Error,Reason}}
    end.

     



-spec delete_user(UserId::number())-> ok  | {error,Error::any()}.
delete_user(UserId)->
    try
        Statement= <<"DELETE FROM  wsuser WHERE id = $1">>,
        {ok,C}=create_connection(),
        {ok,Rows}=epgsql:equery(C,Statement,[UserId]),
        if Rows>0 -> ok ; true -> {error,user_does_not_exist} end
    catch
        Error:Reason -> {error,{Error,Reason}}
    end.



-spec create_topic(TopicData::map())-> {ok,Topic::map()} | already_exists | {error,Error::any()}.
create_topic(_TopicData = #{<<"user_id">> := UserId,<<"name">> := TopicName , <<"user_id">> := UserId})->
    try
        Statement= <<"INSERT INTO  topic(name,created_by) values ($1,$2) RETURNING * ">>,
        {ok,C}=create_connection(),
        {ok,_,Columns,[Values]}=epgsql:equery(C,Statement,[TopicName,UserId]),
         Value=to_map(Columns, tuple_to_list(Values),#{}),
        {ok,Value}
    catch
        Error:Reason -> {error,{Error,Reason}}
    end.
   




-spec delete_topic(Id::integer())-> ok | {error,Error::any()}.
delete_topic(Id)->
    try
        Statement= <<"DELETE FROM  topic WHERE id=$1">>,
        {ok,C}=create_connection(),
        {ok,Rows}=epgsql:equery(C,Statement,[Id]),
        if Rows>0 -> ok ; true -> {error,topic_does_not_exist} end
    catch
    Error:Reason->{error,{Error,Reason}}
end.

-spec subscribe(TopicId::integer(),UserId::integer())-> ok | {error,Error::any()}.
subscribe(TopicId,UserId)->
    Statement= <<"INSERT INTO  user_topic(topic,user_id) values ($1,$2)">>,
    {ok,C}=create_connection(),
    {ok,_}=epgsql:equery(C,Statement,[TopicId,UserId]),
     ok.                 
   


-spec unsubscribe(TopicId::number(),UserId::number())-> ok | not_joined | {error,Error::term()}.
unsubscribe(TopicId,UserId)->
    Statement= <<"DELETE FROM  user_topic WHERE topic_id=$1 and user_id = $2">>,
    {ok,C}=create_connection(),
    {ok,Rows}=epgsql:equery(C,Statement,[TopicId,UserId]),
    if Rows>0 -> ok ; true -> not_joined end.




-spec get_subscriptions_for_topic(TopicId::number())-> {ok,list()} | {error,Reason::term()}.
get_subscriptions_for_topic(TopicId)->
    Statement= <<"SELECT user_id FROM  user_topic WHERE topic_id = $1">>,
    {ok,C}=create_connection(),
    {ok,_,Result}=epgsql:equery(C,Statement,[TopicId]),
    {ok,normalize(Result)}.

-spec get_user_subscriptions(UserId::number())-> {ok,Subscriptions::list()}  | {error,Error::term()}.
get_user_subscriptions(UserId)-> 
    Statement= <<"SELECT topic FROM  user_topic WHERE user_id = $1">>,
    {ok,C}=create_connection(),
    {ok,_,Result}=epgsql:equery(C,Statement,[UserId]),
    io:format("\n query ok\n"),
    {ok,normalize(Result)}.
    
-spec check_if_subscribed(Topic::number(),User::number())->{ok,boolean()}|{error,Error::term()}.
check_if_subscribed(TopicId,UserId)->
    
    Statement= <<"SELECT Count(user_id) FROM  user_topic WHERE topic_id = $1 AND user_id=$2">>,
    {ok,C}=create_connection(),
    {ok,_,[{Count}]}=epgsql:equery(C,Statement,[TopicId,UserId]),
    {ok,Count>0}.

-spec does_topic_exist(TopicId::number())-> true | false | {error,Error::any()}.
does_topic_exist(TopicId)->
    Statement= <<"SELECT 1 FROM topic WHERE id = $1;">>,
    {ok,C}=create_connection(),
    {ok, Result} = epgsql:equery(C, Statement, [TopicId]),
    % Check if the topic exists
    Exists= length(Result)>0,
    Exists.



-spec get_messages(TopicId::integer(),StartIndex::integer(),Count::integer())->{ok,Messages::list()}| {error,Error::term()}.

get_messages(TopicId,StartIndex,Count)->
    Statement= <<"SELECT * FROM message WHERE topic = $1 AND index >= $2 ORDER BY index ASC LIMIT $3">>,
    {ok,C}=create_connection(),
    {ok,_,Result}=epgsql:equery(C,Statement,[TopicId,StartIndex,Count]),
    io:format("\n query ok\n"),
    {ok,normalize(Result)}.

-spec write_chat_message(Message::any())->ok | {error,Error::any()}.
write_chat_message(_Message=#{ 
                   <<"topic">>:=TopicId,
                   <<"user_id">> :=UserId,
                   <<"content">>:=Content,
                   <<"createdAt">>:=CreatedAt,
                   <<"timezone">> :=Timezone})->
    
    Statement= <<"INSERT INTO  message(topic_id,user_id,content,createdAt,timezone) 
                values ($1,$2,$3,$4,$5)">>,
    {ok,C}=create_connection(),
    {ok,_}=epgsql:equery(C,Statement,[TopicId,UserId,Content,CreatedAt,Timezone]),
     ok.   

-spec write_chat_messages(Messages::list())->{ok,Inserted::integer()} | {error,Error::any()}.
write_chat_messages(Messages)->
    Statement= <<"INSERT INTO  message(topic_id,user_id,content,createdAt,timezone) values ($1,$2,$3,$4)">>,
    {ok,C}=create_connection(),
    epgsql:transaction_start(C),
    {ok,Stmt}=epgsql:prepare(C,"insert_statement",Statement),
    ok=write_messages(C,Stmt, Messages),
     ok.   

write_messages(_Connection,_Stmt,[])->ok;
write_messages(Connection,Stmt,[Message|Rest])->
    #{topic_id :=TopicId,user_id :=UserId,content:=Content,
      created_at:=CreatedAt,timezone:=Timezone}=Message,
    epgsql:execute(Connection,Stmt,[TopicId,UserId,Content,CreatedAt,Timezone]),
    write_messages(Connection, Stmt, Rest).

normalize(List)->
    normalize(List,[]).
-spec normalize(List::list(),Acc::list())->list().
normalize([],Acc) ->
    Acc;
normalize([{Head}|Tail], Acc) when is_list([Head|Tail])->
    normalize(Tail,[Head|Acc]).

to_map([],[],Acc)->Acc;
to_map([Column|Columns],[Value|Values],Map)->
    Key=lists:nth(2, tuple_to_list(Column)),
    to_map(Columns,Values,Map#{Key=>Value}).
