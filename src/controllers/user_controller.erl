-module(user_controller).
-include("../../include/domain.hrl").
-export([
         index/1,
         create_user/1,
         get_user/1,
         get_user_by_email/1,
         delete_user/1]).

index(_Req) ->
    {ok, [{message, "Hello world!"}]}.

get_user(_Req=#{parsed_qs := #{<<"id">> := Id}})-> 
case wsapp_server:get_user(binary_to_integer(Id)) of
               
    {ok,User} -> Payload=utils:from_user(User),
                 {json,200,#{<<"Content-Type">> => <<"application/json">>},Payload};
     user_does_not_exist ->{status,404}
end.
get_user_by_email(_Req=#{parsed_qs := #{<<"email">> :=Email}})->
    case wsapp_server:get_user_by_email(Email) of
            {ok,User} ->  Payload=utils:from_user(User),
                         {json,200,#{<<"Content-Type">> => <<"application/json">>},Payload};
            user_does_not_exist ->{status,404}
    end.
    
create_user(_Req=#{json := #{<<"name">>:=Name,<<"email">>:=Email,<<"password">>:=Password}})->
    Params=#create_user_params{name = Name,email = Email,password = Password},
    case wsapp_server:create_user(Params) of
        {ok,User} ->  Payload=utils:from_user(User),
                     {json,200,#{<<"Content-Type">>=> <<"application/json">>},Payload};
        {error,user_already_exists}->{status,409};
        {error,{bad_request,ValidationErrors}}->
            {json,400,#{<<"Content-Type">>=> <<"application/json">>},ValidationErrors};
        {error,Error}->
            {json,500,#{<<"Content-Type">>=> <<"application/json">>},Error};
        Rez-> io:format("~p",[Rez]), 
            {json,500,#{},Rez}
end.
        
delete_user(_Req=#{parsed_qs:=#{<<"id">> := UserId}})->
    io:format(UserId),
    case wsapp_server:delete_user(binary_to_integer(UserId)) of
        ok -> {status,200};
        {error,Error}->
            {json,500,#{<<"Content-Type">>=> <<"application/json">>},Error}
end.  


