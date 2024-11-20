-module(storage).
-include("../include/domain.hrl").
-export([
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
         write_chat_messages/1,
         start/0,
         
         stop/0]).

-define(USER_TABLE,user).
-define(TOPIC_TABLE,topic).
-define(USER_TOPIC_TABLE,user_topic).
-define(MESSAGE_TABLE,message).

-spec start()->ok | {error,term()}.
start()->
    dets:open_file(?USER_TABLE,[{file,"user.dets"}]),
    dets:open_file(?TOPIC_TABLE,[{file,"topic.dets"}]),
    dets:open_file(?USER_TOPIC_TABLE,[{file,"user_topic.dets"}]),
    dets:open_file(?MESSAGE_TABLE,[{file,"message.dets"}]).

-spec stop() -> ok | {error, term()}.
stop()->
   
    dets:close(?USER_TABLE),
    dets:close(?TOPIC_TABLE),
    dets:close(?USER_TOPIC_TABLE),
    dets:close(?MESSAGE_TABLE).

-spec create_user(UserData::domain:create_user_params())->{ok,domain:user()}| already_exists | {error,term()}.
create_user(#create_user_params{email = Email,name = Name,password=Password})->
    case dets:match_object(?USER_TABLE, {'_',Email,'_','_'}) of
        [] -> NewId=erlang:unique_integer([monotonic,positive]),
              NewUser={NewId,Email,Name,Password},
              dets:insert(?USER_TABLE, NewUser),
              {ok,NewUser};
        _ -> already_exists
    end.

-spec get_user(UserId::domain:user_id())-> {ok,domain:user()} | user_does_not_exist | {error,term()}.
get_user(UserId)->
    case dets:lookup(?USER_TABLE,UserId) of
        [{Id,Email,Name,Password}]->{ok,#user{id=Id,email=Email,password=Password,name=Name}};
        []-> user_does_not_exist
end.

-spec get_user_by_email(Email::string())->{ok,domain:user()} | user_does_not_exist | {error,term()}.

get_user_by_email(Email)->
    case dets:match_object(?USER_TABLE,{'_',Email,'_','_'}) of
        [] -> user_does_not_exist;
        [{Id,Email,Name,Password}]-> {ok,#user{id=Id,email=Email,password=Password,name=Name}}
end.

-spec delete_user(UserId::domain:user_id())->ok | {error,term()}.
delete_user(UserId)->
    dets:delete(?USER_TABLE,UserId),
    ok.

-spec create_topic(TopicData::domain:create_topic_params())->{ok,Topic::domain:topic()}| already_exists | {error,Reason::any()}.
create_topic(#create_topic_params{name = TopicName,user_id = UserId})->
    case dets:match_object(?TOPIC_TABLE, {'_',TopicName,'_','_'}) of
        []->
            NewId=erlang:unique_integer([monotonic,positive]),
            CreatedAt=erlang:now(),
            Topic={NewId,TopicName,UserId,CreatedAt},
            dets:insert(?TOPIC_TABLE, Topic),
            {ok,Topic};
        [_] -> already_exists
end.
-spec get_topic(TopicId::integer())->{ok,Topic::domain:topic()} | topic_does_not_exist | {error,Error::any()}.   
get_topic(TopicId)->
    case dets:lookup(?TOPIC_TABLE,TopicId) of 
        [Topic] -> {ok,Topic};
        _ -> topic_does_not_exist
end.

-spec get_topic_by_name(TopicName::string())->{ok,domain:topic()} | topic_does_not_exist | {error,term()}.

get_topic_by_name(TopicName)->
    case dets:match_object(?TOPIC_TABLE,{'_',TopicName,'_','_'}) of
        [] -> user_does_not_exist;
        [Topic]-> {ok,Topic}
end.

-spec delete_topic(Id::domain:topic_id()) -> ok | {error, term()}.
delete_topic(Id) ->
    dets:delete(?TOPIC_TABLE, Id),
    ok.

-spec does_topic_exist(TopicId::domain:topic_id()) -> true | false | {error, Reason::term()}.
does_topic_exist(TopicId) ->
    case dets:lookup(?TOPIC_TABLE, TopicId) of
        [_] -> true;
        [] -> false
    end.

-spec subscribe(TopicId::domain:topic_id(),UserId::domain:user_id())->ok |{error,Reason::term()}.
subscribe(TopicId,UserId)->
    NewId=erlang:unique_integer([monotonic,positive]),
    CreatedAt=erlang:now(),
    UserTopic={NewId,UserId,TopicId,CreatedAt},
    dets:insert(?USER_TOPIC_TABLE,UserTopic),
    ok.

-spec unsubscribe(TopicId::domain:topic_id(),UserId::domain:user_id())-> ok | not_joined | {error,Reason::term()}.
unsubscribe(TopicId,UserId)->
    case dets:match_object(?USER_TOPIC_TABLE, {'_',UserId,TopicId,'_'}) of
        [UserTopic] -> dets:delete(?USER_TOPIC_TABLE,UserTopic),
                       ok;
        [] -> not_joined
end.

-spec get_subscriptions_for_topic(TopicId::domain:topic_id())-> {ok,[domain:user_topic()]} | {error,Reason::term()}.
get_subscriptions_for_topic(TopicId)->
    Result=[#user_topic{id=Id, user_id=UserId, topic_id=TopicId ,created_at =CreatedAt} || {Id,UserId,_,CreatedAt}<-dets:match_object(?USER_TOPIC_TABLE, {'_','_',TopicId,'_'})],
    Result.

-spec get_user_subscriptions(UserId::domain:user_id()) -> {ok,[domain:user_topic()]}|{error,Reason::term()}.
get_user_subscriptions(UserId)->
    Result=[#user_topic{id=Id, user_id=UserId, topic_id=TopicId ,created_at = CreatedAt}||{Id,_,TopicId,CreatedAt}
    <-dets:match_object(?USER_TOPIC_TABLE, {'_',UserId,'_','_'})],
    Result.

-spec check_if_subscribed(TopicId::domain:topic_id(),UserId::domain:user_id())->{ok,boolean()}|{error,Error::term()}.
check_if_subscribed(TopicId,UserId)->
    Result=case dets:match_object(?USER_TOPIC_TABLE, {'_',UserId,TopicId,'_'}) of
        []->false;
        [_]-> true
    end,
    Result.

-spec get_newest_messages(TopicId::domain:topic_id(),Count::number())->{ok,Messages::[domain:message()]}|{error,Reason::term()}.
get_newest_messages(TopicId,Count)->
    Messages=case dets:match(?MESSAGE_TABLE,{'_',TopicId,'_','_','_','_'},Count) of
                '$end_of_table' -> [];
                 {error,Reason}->{error,Reason};
                 Results->
                    {ok,[
                     #message{
                     user_id= UserId ,
                     topic_id= TopicId,
                     content= Message,
                     created_at= CreatedAt,
                     timezone= Timezone}||{Id,UserId,_,Message,CreatedAt,Timezone}<-Results]}
             end,
    Messages.

-spec get_oldest_messages(TopicId::domain:topic_id(),StartIndex::integer(),Count::integer())->{ok,Messages::list()}| {error,Error::term()}.
get_oldest_messages(TopicId,StartIndex,Count)->
    Pattern={'_',TopicId,'_','_','_','_'},
    Results=dets:match_object(?MESSAGE_TABLE, Pattern),
    FilteredMessages=lists:foldl(
             fun({Id,UserId,_TopicId,Message,CreatedAt,Timezone},Acc)->
                case {Id<StartIndex,length(Acc)<Count} of
                    {true,true}->
                        [#message{message_id = Id,
                           user_id=UserId,
                           topic_id=TopicId,
                           content=Message,
                           created_at = CreatedAt,
                           timezone =Timezone}];
                    _ ->Acc
                end
            end,
            [],
            Results),
            {ok,lists:reverse(FilteredMessages)}.

-spec write_chat_message(Message::domain:message())->ok | {error,Error::any()}.
write_chat_message(Input=#message{user_id = UserId,topic_id = TopicId,content = Content,created_at = CreatedAt,timezone = Timezone})->
    Id=erlang:unique_integer([monotonic,positive]),
    Record={Id,UserId,TopicId,Content,CreatedAt,Timezone},
    dets:insert(?MESSAGE_TABLE,Record),
    ok.

-spec write_chat_messages(Messages::[domain:message()])->{ok,Inserted::integer()} | {error,Error::any()}.
write_chat_messages(Messages)->
    try
    Records=[
            {erlang:unique_integer([monotonic, positive]),
             Msg#message.user_id,
             Msg#message.topic_id,
             Msg#message.content,
             Msg#message.created_at,
             Msg#message.timezone}||Msg<-Messages],
    dets:insert(?MESSAGE_TABLE, Records),
    ok
        catch
            _:Error->{error,Error}
        end.



  