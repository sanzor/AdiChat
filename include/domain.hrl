-export_type([
    user_id/0,
    topic_id/0,
    user/0,
    topic/0,
    create_user_params/0,
    create_topic_params/0]
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
    user_id::number()
}).

-type create_topic_params()::#create_topic_params{
    name::string(),
    user_id::number()
}.

-record(user,{
    id::integer(),
    name::string(),
    email::string(),
    password::string()
}).


-type user()::#user{
    id::integer(),
    name::string(),
    email::string(),
    password::string()
}.

-record(topic,{
    name::string(),
    created_by::integer(),
    created_at::string()
}).
-type topic()::#topic{
}.




