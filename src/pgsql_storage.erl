-module(pgsql_storage).
-include("../include/domain.hrl").
-export([
         to_map/3,
         create_user/1,
         get_user/1,
         get_user_by_email/1,
         delete_user/1,
         create_topic/1,
         get_topic_by_name/1,
         get_topic/1,
         delete_topic/1,
         does_topic_exist/1,
         subscribe/2,
         unsubscribe/2,
         check_if_subscribed/2,
         get_subscriptions_for_topic/1,
         get_user_subscriptions/1,
         get_oldest_messages/3,
         get_newest_messages/2,
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
    
-spec get_user(UserId::domain:user_id())->{ok,User::domain:user()} | {error,Error::any()}.
get_user(UserId)->
    Statement= <<"Select * FROM wsuser WHERE id=$1">>,
    {ok,C}=create_connection(),
    {ok,Columns,Values}=epgsql:equery(C,Statement,[UserId]),
    Value=case to_records(Columns, Values) of
        [] ->user_does_not_exist;
        Else->  UserMap=lists:nth(1, Else),
                User=utils:to_user(UserMap),
                {ok,User}
    end,
    Value.

-spec get_user_by_email(UserId::domain:user_id())->{ok,User::domain:user()} | user_does_not_exist.
get_user_by_email(Email)->
    Statement= <<"Select * FROM wsuser WHERE email=$1">>,
    {ok,C}=create_connection(),
    {ok,Columns,Values}=epgsql:equery(C,Statement,[Email]),
    Value=case to_records(Columns, Values) of
        [] ->user_does_not_exist;
        Else->    UserMap=lists:nth(1, Else),
                  User=utils:to_user(UserMap),
                  {ok,User}
    end,
    Value.


-spec create_user(UserData::domain:create_user_params())-> {ok,User::domain:user()} | already_exists | {error,Error::any()}.
create_user(_UserData=#create_user_params{email = Email,name = Name,password = Password})->
        
        Statement= <<"INSERT INTO  wsuser(email,password,name) values ($1,$2,$3) RETURNING *">>,
        {ok,C}=create_connection(),
        Result=case epgsql:equery(C,Statement,[Email,Password,Name]) of
                    
                    {error,{error,error,<<"23505">>,unique_violation,_,_}}->already_exists;
                    {ok,_,Columns,Values}->
                                    
                                  Value=case to_records(Columns, Values) of
                                            [] -> user_does_not_exist;
                                            Else-> 
                                                    UserMap=lists:nth(1, Else),
                                                    User=utils:to_user(UserMap),
                                                    {ok,User}
                                                   
                                        end,
                                  Value
                    end,
        Result.

   
       
     



-spec delete_user(UserId::domain:user_id())-> ok  | {error,Error::any()}.
delete_user(UserId)->
    try
        Statement= <<"DELETE FROM  wsuser WHERE id = $1">>,
        {ok,C}=create_connection(),
        {ok,Rows}=epgsql:equery(C,Statement,[UserId]),
        if Rows>0 -> ok ; true -> {error,user_does_not_exist} end
    catch
        Error:Reason -> {error,{Error,Reason}}
    end.


-spec get_topic_by_name(TopicName::binary())->{ok,Topic::domain:topic()} | topic_does_not_exist | {error,Error::any()}.
get_topic_by_name(TopicName)->
    Statement= <<"Select * FROM topic WHERE name=$1">>,
    {ok,C}=create_connection(),
    {ok,Columns,Values}=epgsql:equery(C,Statement,[TopicName]),
    Value=case to_records(Columns, Values) of
        [] ->topic_does_not_exist;
        Else->  TopicMap=lists:nth(1, Else),
                Topic=Topic=utils:to_topic(TopicMap),
                {ok,Topic}
    end,
    Value.

-spec get_topic(TopicId::integer())->{ok,Topic::domain:topic()} | topic_does_not_exist | {error,Error::any()}.   
get_topic(TopicId)->
    Statement= <<"Select * FROM topic WHERE id=$1">>,
    {ok,C}=create_connection(),
    {ok,Columns,Values}=epgsql:equery(C,Statement,[TopicId]),
    Value=case to_records(Columns, Values) of
        [] ->topic_does_not_exist;
        Else->  TopicMap=lists:nth(1, Else),
                Topic=Topic=utils:to_topic(TopicMap),
                {ok,Topic}  
    end,
    Value.
-spec create_topic(TopicData::domain:create_topic_params())-> {ok,Topic::domain:topic()} | already_exists | {error,Error::any()}.
create_topic(_TopicData =#create_topic_params{user_id = UserId,name = TopicName})->
    
        Statement= <<"INSERT INTO  topic(name,created_by) values ($1,$2) RETURNING * ">>,
        {ok,C}=create_connection(),
        Result=case epgsql:equery(C,Statement,[TopicName,UserId]) of
                   
                    {error,{error,error,<<"23505">>,unique_violation,_,_}}->already_exists;
                    {ok,_,Columns,Values}->
                                  Value=case to_records(Columns, Values) of
                                  [] ->user_does_not_exist;
                                  Else-> TopicMap=lists:nth(1, Else),
                                         Topic=utils:to_topic(TopicMap),
                                         {ok,Topic}
                                  end,
                                  Value
                    end,
        Result.
   
   




-spec delete_topic(Id::domain:topic_id())-> ok | {error,Error::any()}.
delete_topic(Id)->
    try
        Statement= <<"DELETE FROM  topic WHERE id=$1">>,
        {ok,C}=create_connection(),
        {ok,Rows}=epgsql:equery(C,Statement,[Id]),
        if Rows>0 -> ok ; true -> {error,topic_does_not_exist} end
    catch
    Error:Reason->{error,{Error,Reason}}
end.

-spec subscribe(TopicId::domain:topic_id(),UserId::domain:user_id())-> ok | {error,Error::any()}.
subscribe(TopicId,UserId)->
    Statement= <<"INSERT INTO  user_topic(topic_id,user_id) values ($1,$2)">>,
    {ok,C}=create_connection(),
    {ok,_}=epgsql:equery(C,Statement,[TopicId,UserId]),
     ok.                 
   


-spec unsubscribe(TopicId::domain:topic_id(),UserId::domain:user_id())-> ok | not_joined | {error,Error::term()}.
unsubscribe(TopicId,UserId)->
    Statement= <<"DELETE FROM  user_topic WHERE topic_id=$1 and user_id = $2">>,
    {ok,C}=create_connection(),
    {ok,Rows}=epgsql:equery(C,Statement,[TopicId,UserId]),
    if Rows>0 -> ok ; true -> not_joined end.




-spec get_subscriptions_for_topic(TopicId::domain:topic_id())-> {ok,list()} | {error,Reason::term()}.
get_subscriptions_for_topic(TopicId)->
    Statement= <<"SELECT user_id FROM  user_topic WHERE topic_id = $1">>,
    {ok,C}=create_connection(),
    {ok,Columns,Values}=epgsql:equery(C,Statement,[TopicId]),
    {ok,to_records(Columns, Values)}.

-spec get_user_subscriptions(UserId::domain:user_id())-> {ok,Subscriptions::list()}  | {error,Error::term()}.
get_user_subscriptions(UserId)-> 
    Statement= <<"SELECT topic.id,topic.name FROM  user_topic INNER JOIN topic on user_topic.topic_id=topic.id WHERE user_topic.user_id = $1">>,
    {ok,C}=create_connection(),
    {ok,Columns,Values}=epgsql:equery(C,Statement,[UserId]),
     {ok,to_records(Columns, Values)}.
    
-spec check_if_subscribed(Topic::domain:topic_id(),UserId::domain:user_id())->{ok,boolean()}|{error,Error::term()}.
check_if_subscribed(TopicId,UserId)->
    
    Statement= <<"SELECT Count(user_id) FROM  user_topic WHERE topic_id = $1 AND user_id=$2">>,
    {ok,C}=create_connection(),
    {ok,_,[{Count}]}=epgsql:equery(C,Statement,[TopicId,UserId]),
    {ok,Count>0}.

-spec does_topic_exist(TopicId::domain:topic_id())-> true | false | {error,Error::any()}.
does_topic_exist(TopicId)->
    Statement= <<"SELECT 1 FROM topic WHERE id = $1;">>,
    {ok,C}=create_connection(),
    {ok, Result} = epgsql:equery(C, Statement, [TopicId]),
    % Check if the topic exists
    Exists= length(Result)>0,
    Exists.



-spec get_oldest_messages(TopicId::domain:topic_id(),StartIndex::integer(),Count::integer())->{ok,Messages::list()}| {error,Error::term()}.

get_oldest_messages(TopicId,StartIndex,Count)->
    Statement="SELECT * FROM message WHERE topic_id = $1 AND id < $2 ORDER BY id DESC LIMIT $3;",
    {ok,C}=create_connection(),
    {ok,Columns,Values}=epgsql:equery(C,Statement,[TopicId,StartIndex,Count]),
    {ok,to_records(Columns, Values)}.

-spec get_newest_messages(TopicId::domain:topic_id(),Count::integer())->{ok,Messages::list()}| {error,Error::term()}.
get_newest_messages(TopicId,Count)->
    Statement="SELECT * FROM message WHERE topic_id = $1 ORDER BY id DESC LIMIT $2;",
    {ok,C}=create_connection(),
    {ok,Columns,Values}=epgsql:equery(C,Statement,[TopicId,Count]),
    {ok,to_records(Columns, Values)}.

-spec write_chat_message(Message::domain:message())->ok | {error,Error::any()}.
write_chat_message(Input=#message{user_id = UserId,topic_id = TopicId,content = Content,created_at = CreatedAt,timezone = Timezone})->

    Statement= <<"INSERT INTO  message(topic_id,user_id,message,created_at,timezone) 
                values ($1,$2,$3,$4,$5)">>,
    io:format("\nInput is ~p\n",[Input]),
    {ok,C}=create_connection(),
    {ok,_}=case epgsql:equery(C,Statement,[TopicId,UserId,Content,CreatedAt,Timezone]) of
             {ok,_} -> {ok,1};
             {error,Reason}->io:format("\nError ~p",[Reason]),
                             Reason
           end,
     ok.   

-spec write_chat_messages(Messages::[domain:message()])->{ok,Inserted::integer()} | {error,Error::any()}.
write_chat_messages(Messages)->
    Statement= <<"INSERT INTO  message(topic_id,user_id,content,createdAt,timezone) values ($1,$2,$3,$4)">>,
    {ok,C}=create_connection(),
    epgsql:transaction_start(C),
    {ok,Stmt}=epgsql:prepare(C,"insert_statement",Statement),
    ok=write_messages(C,Stmt, Messages),
    ok.   

write_messages(_Connection,_Stmt,[])->ok;
write_messages(Connection,Stmt,[Message|Rest])->
    #message{topic_id =TopicId,user_id =UserId,content=Content,
      created_at=CreatedAt,timezone=Timezone}=Message,
    epgsql:execute(Connection,Stmt,[TopicId,UserId,Content,CreatedAt,Timezone]),
    write_messages(Connection, Stmt, Rest).



to_records(_,[])->[];
to_records(Columns,Values)->
    to_records(Columns, Values,[]).
to_records(_,[],Acc)->Acc;
to_records(Columns,[CurrentRow|Rest],Acc)->
    to_records(Columns,Rest, [to_map(Columns,tuple_to_list(CurrentRow),#{})|Acc]).

to_map([],[],Acc)->Acc;
to_map([Column|Columns],[Value|Values],Map)->
    Key=lists:nth(2, tuple_to_list(Column)),
    to_map(Columns,Values,Map#{Key=>Value}).
