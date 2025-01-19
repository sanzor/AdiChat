-module(utils).
-export([from_user/1,from_topic/1,to_topic/1,to_user/1,from_user_topic/1]).
-export([datetime_to_string/1]).
-include("../include/domain.hrl").
-spec from_user(User::domain:user())->map().
from_user(User=#user{id = Id,email = Email,name = Name,password=Password})->
    io:format("\nSugi User value:~p",[User]),
    #{id=>Id,email=>Email,name=>Name,password=>Password}.

-spec from_topic(Topic::domain:topic())->map().
from_topic(_=#topic{id = Id,name=Name,created_by = CreatedBy})->
    #{id=>Id,name=>Name,created_by=>CreatedBy}.

-spec from_user_topic(UserTopic::domain:user_topic())->map().
from_user_topic(UserTopic=#user_topic{id = Id,topic_id = TopicId,user_id = UserId})->
    #{id=>Id,topic_id=>TopicId,user_id=>UserId}.

-spec to_user(User::map())->domain:user().
to_user(_=#{<<"id">>:=Id,<<"name">>:=Name,<<"email">>:=Email,<<"password">>:=Password})->
    #user{id = Id,name = Name,email = Email,password = Password}.

-spec to_topic(Topic::map())->domain:topic().
to_topic(_=#{<<"id">>:=Id,<<"name">>:=Name,<<"created_by">>:=CreatedBy})->
    #topic{id = Id,name = Name,created_by = CreatedBy}.

datetime_to_string({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", [Year, Month, Day, Hour, Minute, Second])).
