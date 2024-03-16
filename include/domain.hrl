-export_type([user_id/0,create_user_params/0,create_topic_params/0]).
-type user_id()::integer().

-type create_user_params()::#create_user_params{
    name::string(),
    email::string(),
    password::string()
}.

-record(create_user_params,{
    name::string(),
    email::string(),
    password::string()
}).

-type create_topic_params()::#create_topic_params{
    name::string(),
    user_id::integer()
}.

-record(create_topic_params,{
    name::string(),
    user_id::integer()
}).


