-module(storage).
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

-define(USER_TABLE,"wsuser.dets").
-define(TOPIC_TABLE,"topic.dets").
-define(USER_TOPIC_TABLE,"user_topic.dets").
-define(MESSAGE_TABLE,"message.dets").

-spec start()->ok | {error,term()}.
start()->
    dets:open_file(?USER_TABLE),
    dets:open_file(?TOPIC_TABLE),
    dets:open_file(?USER_TOPIC_TABLE),
    dets:open_file(?MESSAGE_TABLE).

-spec stop() -> ok | {error, term()}.
stop()->
    dets:close(?USER_TABLE),
    dets:close(?TOPIC_TABLE),
    dets:close(?USER_TOPIC_TABLE),
    dets:close(?MESSAGE_TABLE).

-spec create_user(UserData::domain:create_user_params())->{ok,domain:user()}| already_exists | {error,term()}.
create_user(#create_user_params{email = Email,name = Name,password=Password})->
    case dets:match_object(?USER_TABLE, {'_','_',Email,'_'}) of
        [] -> NewId=erlang:unique_integer([monotonic,positive]),
              NewUser={NewId,Email,Password,Name},
              dets:insert(?USER_TABLE, NewUser),
              {ok,NewUser};
        _ -> already_exists
    end.

-spec get_user(UserId::domain:user_id())-> {ok,domain:user()} | user_does_not_exist | {error,term()}.
get_user(UserId)->
    case dets:lookup(?USER_TABLE,UserId) of
        [User]->{ok,User};
        []-> user_does_not_exist
end.

-spec get_user_by_email(Email::string())->{ok,domain:user()} | user_does_not_exist | {error,term()}.

get_user_by_email(Email)->
    case dets:match_object(?USER_TABLE,{'_',Email,'_','_'}) of
        [] -> user_does_not_exist;
        [User]-> {ok,User}
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
              