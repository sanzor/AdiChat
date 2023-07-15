-module(validator).

-export([validate_user_data/1]).
-export([validate_topic_data/1]).


validate_user_data(_UserData=#{name := Name}) when is_binary(Name)-> {false,[{invalid_name,Name}]};
validate_user_data(_UserData)->true.


validate_topic_data(_TopicData=#{name := Name}) when is_binary(Name)-> {false,[{invalid_name,Name}]};
validate_topic_data(_TopicData)->true.

