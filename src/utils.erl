-module(utils).
-export([from_user/1,from_topic/1,to_topic/1,to_user/1]).
-include("../include/domain.hrl").
-spec from_user(User::domain:user())->map().
from_user(_=#user{id = Id,email = Email,name = Name,password=Password})->
    #{id=>Id,email=>Email,name=>Name,password=>Password}.

from_topic(_=#topic{id = Id,name=Name,created_by = CreatedBy})->
    #{id=>Id,name=>Name,creted_by=>CreatedBy}.

to_user(_=#{id:=Id,name:=Name,email:=Email,password:=Password})->
    #user{id = Id,name = Name,email = Email,password = Password}.
to_topic(_=#{id:=Id,name:=Name,created_by:=CreatedBy})->
    #topic{id = Id,name = Name,created_by = CreatedBy}.