-module(storage).
-include("../include/domain.hrl").
-export([
         create_user/1,
         get_user/1,
         get_user_by_email/1,
         delete_user/1,
         verify_user/1,
         login_user/2,
         create_verification_token/1,
         get_verification_token/1,
         delete_verification_token/1,
         create_topic/1,
         get_topic_by_name/1,
         get_topic/1,
         delete_topic/1,
         does_topic_exist/1,
         subscribe/2,
         unsubscribe/2,
         check_if_subscribed/2,
         get_subscriptions_for_topic/1,
         get_recipients/1,
         get_user_subscriptions/1,
         get_oldest_messages/3,
         get_newest_messages/2,
         write_chat_message/1,
         write_chat_messages/1,
         update_message_status/2,
         start/0,
         stop/0]).

-define(USER_TABLE,user).
-define(TOPIC_TABLE,topic).
-define(USER_TOPIC_TABLE,user_topic).
-define(MESSAGE_TABLE,message).
-define(VERIFICATION_TOKEN_TABLE,verification_token).

-spec start()->ok | {error,term()}.
start()->
    dets:open_file(?USER_TABLE,[{file,"user.dets"}]),
    dets:open_file(?TOPIC_TABLE,[{file,"topic.dets"}]),
    dets:open_file(?USER_TOPIC_TABLE,[{file,"user_topic.dets"}]),
    dets:open_file(?MESSAGE_TABLE,[{file,"message.dets"}]),
    dets:open_file(?VERIFICATION_TOKEN_TABLE,[{file,"verification_token.dets"}]).

-spec stop() -> ok | {error, term()}.
stop()->
    dets:close(?USER_TABLE),
    dets:close(?TOPIC_TABLE),
    dets:close(?USER_TOPIC_TABLE),
    dets:close(?MESSAGE_TABLE),
    dets:close(?VERIFICATION_TOKEN_TABLE).

%% User tuple format: {Id, Email, Name, Password, IsVerified, IsActive}

-spec create_user(UserData::domain:create_user_params())->{ok,domain:user()}| already_exists | {error,term()}.
create_user(#create_user_params{email = Email,name = Name,password=Password})->
    case dets:match_object(?USER_TABLE, {'_',Email,'_','_','_','_'}) of
        [] -> NewId=erlang:unique_integer([monotonic,positive]),
              NewUser={NewId,Email,Name,Password,false,true},
              dets:insert(?USER_TABLE, NewUser),
              {ok,#user{id=NewId,email=Email,name=Name,password=Password,
                        is_verified=false,is_active=true}};
        _ -> already_exists
    end.

-spec get_user(UserId::domain:user_id())-> {ok,domain:user()} | user_does_not_exist | {error,term()}.
get_user(UserId)->
    case dets:lookup(?USER_TABLE,UserId) of
        [{Id,Email,Name,Password,IsVerified,IsActive}]->
            {ok,#user{id=Id,email=Email,password=Password,name=Name,
                      is_verified=IsVerified,is_active=IsActive}};
        []-> user_does_not_exist
end.

-spec get_user_by_email(Email::string())->{ok,domain:user()} | user_does_not_exist | {error,term()}.
get_user_by_email(Email)->
    case dets:match_object(?USER_TABLE,{'_',Email,'_','_','_','_'}) of
        [] -> user_does_not_exist;
        [{Id,Email,Name,Password,IsVerified,IsActive}]->
            {ok,#user{id=Id,email=Email,password=Password,name=Name,
                      is_verified=IsVerified,is_active=IsActive}}
end.

-spec login_user(Email::binary(),Password::binary()) ->
    {ok,domain:user()} | user_does_not_exist | invalid_password | not_verified | not_active.
login_user(Email,Password)->
    case get_user_by_email(Email) of
        user_does_not_exist -> user_does_not_exist;
        {ok,User=#user{password=StoredPassword,is_verified=IsVerified,is_active=IsActive}} ->
            case StoredPassword of
                Password ->
                    case {IsVerified,IsActive} of
                        {false,_} -> not_verified;
                        {_,false} -> not_active;
                        {true,true} -> {ok,User}
                    end;
                _ -> invalid_password
            end
    end.

-spec verify_user(UserId::domain:user_id()) -> {ok,domain:user()} | user_does_not_exist | {error,term()}.
verify_user(UserId)->
    case dets:lookup(?USER_TABLE,UserId) of
        [{Id,Email,Name,Password,_IsVerified,IsActive}]->
            UpdatedUser={Id,Email,Name,Password,true,IsActive},
            dets:insert(?USER_TABLE, UpdatedUser),
            {ok,#user{id=Id,email=Email,password=Password,name=Name,
                      is_verified=true,is_active=IsActive}};
        []-> user_does_not_exist
    end.

%% Verification token tuple format: {Token, UserId, CreatedAt}

-spec create_verification_token(UserId::domain:user_id()) -> {ok,binary()}.
create_verification_token(UserId)->
    %% Delete any existing tokens for this user
    Existing = dets:match_object(?VERIFICATION_TOKEN_TABLE, {'_',UserId,'_'}),
    [dets:delete(?VERIFICATION_TOKEN_TABLE, T) || {T,_,_} <- Existing],
    %% Generate a new token
    Token = base64:encode(crypto:strong_rand_bytes(32)),
    CreatedAt = erlang:system_time(second),
    dets:insert(?VERIFICATION_TOKEN_TABLE, {Token, UserId, CreatedAt}),
    {ok, Token}.

-spec get_verification_token(Token::binary()) ->
    {ok, domain:verification_token()} | token_not_found | token_expired.
get_verification_token(Token)->
    case dets:lookup(?VERIFICATION_TOKEN_TABLE, Token) of
        [{Token, UserId, CreatedAt}] ->
            Now = erlang:system_time(second),
            %% Token expires after 24 hours
            case Now - CreatedAt > 86400 of
                true -> token_expired;
                false -> {ok, #verification_token{token=Token,user_id=UserId,created_at=CreatedAt}}
            end;
        [] -> token_not_found
    end.

-spec delete_verification_token(Token::binary()) -> ok.
delete_verification_token(Token)->
    dets:delete(?VERIFICATION_TOKEN_TABLE, Token),
    ok.

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
            {ok,#topic{id=NewId,name = TopicName,created_at = CreatedAt,created_by = UserId }};
        [_] -> already_exists
end.
-spec get_topic(TopicId::integer())->{ok,Topic::domain:topic()} | topic_does_not_exist | {error,Error::any()}.
get_topic(TopicId)->
    case dets:lookup(?TOPIC_TABLE,TopicId) of
        [{NewId,TopicName,UserId,CreatedAt}] -> {ok,#topic{id=NewId,name = TopicName,created_at = CreatedAt,created_by = UserId }};
        _ -> topic_does_not_exist
end.

-spec get_topic_by_name(TopicName::string())->{ok,domain:topic()} | topic_does_not_exist | {error,term()}.

get_topic_by_name(TopicName)->
    case dets:match_object(?TOPIC_TABLE,{'_',TopicName,'_','_'}) of
        [] -> topic_does_not_exist;
        [{NewId,TopicName,UserId,CreatedAt}]-> {ok,#topic{id=NewId,name = TopicName,created_at = CreatedAt,created_by = UserId }}
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
        [{Id,_,_,_}] -> dets:delete(?USER_TOPIC_TABLE,Id),
                       ok;
        [] -> not_joined
end.

-spec get_subscriptions_for_topic(TopicId::domain:topic_id())-> {ok,[domain:user_topic()]} | {error,Reason::term()}.
get_subscriptions_for_topic(TopicId)->
    Result=[#user_topic{id=Id, user_id=UserId, topic_id=TopicId ,created_at =CreatedAt} || {Id,UserId,_,CreatedAt}<-dets:match_object(?USER_TOPIC_TABLE, {'_','_',TopicId,'_'})],
    {ok,Result}.

-spec get_recipients(TopicId::domain:topic_id())->{ok,[domain:user()]}| {error,Reason::any()}.
get_recipients(TopicId)->
    {ok,UserTopics}=get_subscriptions_for_topic(TopicId),
    Recipients=[get_user(UserId)||#user_topic{user_id = UserId}<-UserTopics],
    {ok,Recipients}.

-spec get_user_subscriptions(UserId::domain:user_id()) -> {ok,[domain:topic()]}|{error,Reason::term()}.
get_user_subscriptions(UserId)->
    Result = [
            #topic{id = TopicId, name = Name}
            || {_, _, TopicId, _} <- dets:match_object(?USER_TOPIC_TABLE, {'_', UserId, '_', '_'}),
               {_, Name, _, _} <- dets:match_object(?TOPIC_TABLE, {TopicId, '_', '_', '_'})
            ],

    {ok,Result}.

-spec check_if_subscribed(TopicId::domain:topic_id(),UserId::domain:user_id())->{ok,boolean()}|{error,Error::term()}.
check_if_subscribed(TopicId,UserId)->
    Result=case dets:match_object(?USER_TOPIC_TABLE, {'_',UserId,TopicId,'_'}) of
        []->false;
        [R]-> io:format("\nResult from check if subscribed: \n~p",[R]), true
    end,
    Result.

-spec get_newest_messages(TopicId::domain:topic_id(),Count::number())->{ok,Messages::[domain:message()]}|{error,Reason::term()}.
get_newest_messages(TopicId,Count)->
    Result=case dets:match(?MESSAGE_TABLE,{'_','_',TopicId,'_','_','_','_','_'},Count) of
                '$end_of_table' -> {ok,[]};
                 {error,Reason}->{error,Reason};
                 Results->
                    {ok,[
                     #message{
                     message_id = Id,
                     temp_id = TempId,
                     user_id= UserId ,
                     topic_id= TopicId,
                     content= Message,
                     created_at= CreatedAt,
                     timezone= Timezone,
                     status = Status}||{Id,TempId,UserId,_,Message,CreatedAt,Timezone,Status}<-Results]}
             end,
    Result.

-spec get_oldest_messages(TopicId::domain:topic_id(),StartIndex::integer(),Count::integer())->{ok,Messages::list()}| {error,Error::term()}.
get_oldest_messages(TopicId,StartIndex,Count)->
    Pattern={'_',TopicId,'_','_','_','_'},
    Results=dets:match_object(?MESSAGE_TABLE, Pattern),
    FilteredMessages=lists:foldl(
             fun({Id,TempId,UserId,_TopicId,Message,CreatedAt,Timezone,Status},Acc)->
                case {Id<StartIndex,length(Acc)<Count} of
                    {true,true}->
                        [#message{
                           message_id = Id,
                           temp_id=TempId,
                           user_id=UserId,
                           topic_id=TopicId,
                           content=Message,
                           created_at = CreatedAt,
                           timezone =Timezone,
                           status = Status}];
                    _ ->Acc
                end
            end,
            [],
            Results),
            {ok,lists:reverse(FilteredMessages)}.

-spec write_chat_message(Message::domain:message_dto())->{ok,domain:message()} | {error,Error::any()}.
write_chat_message(#message_dto{user_id = UserId, topic_id = TopicId, content = Content,temp_id = TempId}) ->
    Id = erlang:unique_integer([monotonic, positive]),

    % Get system time in seconds (UNIX timestamp)
    CreatedAt = erlang:system_time(second),
    Status= <<"sent">>,
    % Store raw timestamp in DETS
    Record = {Id,TempId, UserId, TopicId, Content, CreatedAt, "utc+2",Status},
    dets:insert(?MESSAGE_TABLE, Record),

    % Convert UNIX timestamp (seconds) to Erlang datetime
    DateTime = calendar:gregorian_seconds_to_datetime(CreatedAt + 62167219200),
    FormattedDateTime = case DateTime of
        {{_, _, _}, {_, _, _}} -> datetime_to_string(DateTime);
        _ -> DateTime  % If it's already a string, return it as is
end,
    {ok, #message{
        message_id = Id,
        temp_id = TempId,
        topic_id = TopicId,
        user_id = UserId,
        content = Content,
        created_at = FormattedDateTime,
        status = Status
    }}.

-spec update_message_status(MessageId::domain:message_id(), Status::binary()) ->
        {ok, domain:message()} | {error, any()}.

update_message_status(MessageId, NewStatus) ->
        case dets:lookup(?MESSAGE_TABLE, MessageId) of
            [{MessageId, TempId, UserId, TopicId, Message, CreatedAt, Timezone, _OldStatus}] ->
                % Create updated record with new status
                UpdatedMessage = {MessageId, TempId, UserId, TopicId, Message, CreatedAt, Timezone, NewStatus},

                % Insert the updated message back into dets
                dets:insert(?MESSAGE_TABLE, UpdatedMessage),

                {ok, UpdatedMessage};

            [] -> {error, not_found};
            Error -> {error, Error}
        end.

datetime_to_string({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
                                [Year, Month, Day, Hour, Minute, Second])).

-spec write_chat_messages(Messages::[domain:message()])->{ok,Inserted::integer()} | {error,Error::any()}.
write_chat_messages(Messages)->
    try
    Records=[
            {erlang:unique_integer([monotonic, positive]),
             Msg#message.temp_id,
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
