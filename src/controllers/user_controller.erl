-module(user_controller).
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
    {ok,User} ->{json,200,#{<<"Content-Type">> => <<"application/json">>},User};
     user_does_not_exist ->{status,404}
end.
get_user_by_email(_Req=#{parsed_qs := #{<<"email">> :=Email}})->
    case wsapp_server:get_user_by_email(Email) of
            {ok,User} ->{json,200,#{<<"Content-Type">> => <<"application/json">>},User};
            user_does_not_exist ->{status,404}
    end.
    
create_user(_Req=#{json := Json})->
    io:format("\nJson is: ~p\n",[Json]),
    case wsapp_server:create_user(Json) of
        {ok,User} -> {json,200,#{<<"Content-Type">>=> <<"application/json">>},User};
        {error,user_already_exists}->{status,409};
        {error,{bad_request,ValidationErrors}}->
            {json,400,#{<<"Content-Type">>=> <<"application/json">>},ValidationErrors};
        {error,Error}->
            {json,500,#{<<"Content-Type">>=> <<"application/json">>},Error}
end.
        
delete_user(_Req=#{parsed_qs:=#{<<"id">> := UserId}})->
    io:format(UserId),
    case wsapp_server:delete_user(binary_to_integer(UserId)) of
        ok -> {status,200};
        {error,Error}->
            {json,500,#{<<"Content-Type">>=> <<"application/json">>},Error}
end.  


