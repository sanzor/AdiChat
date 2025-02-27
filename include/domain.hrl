-export_type([
    user_id/0,
    topic_id/0,
    user/0,
    topic/0,
    topic_with_messages/0,
    create_user_params/0,
    create_topic_params/0,
    subscribe_result/0,
    message/0,
    message_dto/0]
).
-type user_id()::number().
-type topic_id()::number().

-record(create_user_params,{
    name::string(),
    email::string(),
    password::string()
}).
-type create_user_params()::#create_user_params{
    name::string(),
    email::string(),
    password::string()
}.

-record(create_topic_params,{
    name::string(),
    user_id::user_id()
}).

-type create_topic_params()::#create_topic_params{
    user_id::user_id(),
    name::string()
}.

-record(user,{
    id::user_id(),
    name::string(),
    email::string(),
    password::string()
}).


-type user()::#user{
    id::user_id(),
    name::string(),
    email::string(),
    password::string()
}.


-record(topic,{
    id::topic_id(),
    name::string(),
    created_by::integer(),
    created_at::string()
}).
-type topic()::#topic{
    id::topic_id(),
    name::string(),
    created_by::integer(),
    created_at::string()
}.

-record(subscribe_result,{
    result::topic() ,
    subscriber_id::user_id()
}).
-type subscribe_result()::#subscribe_result{
    result::topic() ,
    subscriber_id::user_id()
}.

-record(user_topic,{
    id::user_topic_id(),
    topic_id::topic_id(),
    user_id::user_id(),
    created_at::string()
}).
-type user_topic()::#user_topic{
    id::user_topic_id(),
    topic_id::topic_id(),
    user_id::user_id(),
    created_at::string()
}.
-type user_topic_id()::integer().
-record(message,{
    message_id::message_id(),
    content::message_content(),
    user_id::user_id(),
    topic_id::topic_id(),
    created_at::string(),
    timezone::string()
}).

-type message_dto()::#message_dto{
    content::message_content(),
    user_id::user_id(),
    topic_id::topic()
}.

-record(message_dto,{
    user_id::user_id(),
    topic_id::topic_id(),
    content::message_content()
}).
-type message()::#message{
    message_id::message_id(),
    content::message_content(),
    user_id::user_id(),
    topic_id::topic_id(),
    created_at::string(),
    timezone::binary()
}.
-record(topic_with_messages,{
    topic::topic(),
    messages::[message()]
}).
-type topic_with_messages()::#topic_with_messages{
    topic::topic(),
    messages::[message()]
}.


-type message_content()::string().
-type message_id()::number().

