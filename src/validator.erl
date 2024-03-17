-module(validator).
-include("../include/domain.hrl").
-export([validate_user_data/1]).
-export([validate_topic_data/1]).

-spec validate_user_data(UserData::domain:create_user_params())->{false,[{invalid_name,Name::binary()}]} | true.
validate_user_data(_UserData=#create_user_params{name = Name}) when is_binary(Name)-> {false,[{invalid_name,Name}]};
validate_user_data(_UserData)->true.


validate_topic_data(_TopicData=#create_topic_params{name = Name}) when is_binary(Name)-> {false,[{invalid_name,Name}]};
validate_topic_data(_TopicData)->true.

