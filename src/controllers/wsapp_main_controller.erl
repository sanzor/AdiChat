-module(wsapp_main_controller).
-export([
         index/1,
         create_user/1,
         delete_user/1,
         create_topic/1,
         delete_topic/1,
         publish/1,
         subscribe/1,
         unsubscribe/1
        ]).

index(_Req) ->
    {ok, [{message, "Hello world!"}]}.

create_user(_Req=#{json := UserData})->
    case wsapp_server:create_user(UserData) of
        {ok,User} -> {json,200,#{<<"Content-Type">>=> <<"application/json">>},User};
        {error,{bad_request,ValidationErrors}}->
            {json,400,#{<<"Content-Type">>=> <<"application/json">>},ValidationErrors};
        {error,Error}->
            {json,500,#{<<"Content-Type">>=> <<"application/json">>},Error}
end.
        
delete_user(_Req=#{parsed_qs:=#{id := UserId}})->
    case wsapp_server:delete_user(UserId) of
        ok -> {status,200};
        {error,Error}->
            {json,500,#{<<"Content-Type">>=> <<"application/json">>},Error}
end.  

create_topic(_Req=#{json := TopicData})->
    case wsapp_server:create_topic(TopicData) of
        {ok,User} -> {json,200,#{<<"Content-Type">>=> <<"application/json">>},User};
        {error,{bad_request,ValidationErrors}}->
            {json,400,#{<<"Content-Type">>=> <<"application/json">>},ValidationErrors};
        {error,Error}->
            {json,500,#{<<"Content-Type">>=> <<"application/json">>},Error}
end.
        
delete_topic(_Req=#{parsed_qs:=#{id := TopicId}})->
    case wsapp_server:delete_topic(TopicId) of
        ok -> {status,200};
        {error,Error}->
            {json,500,#{<<"Content-Type">>=> <<"application/json">>},Error}
end.  
publish(_Req=#{json := #{ <<"topic">> := Topic , <<"sender">> := _Sender , <<"message">> := Message}})->
    ok=wsapp_server:publish(Topic,Message),
    {status,200}.

subscribe(_Req=#{parsed_qs := #{ <<"user">> :=User, <<"topic">> := Topic}})->
    ok=wsapp_server:subscribe(User,Topic),
    {status,200}.

unsubscribe(_Req=#{parsed_qs := #{ <<"user">> :=User, <<"topic">> := Topic}})->
    ok=wsapp_server:unsubscribe(User,Topic),
    {status,200}.

get_messages(Req=#{parsed_qs := #{ <<"topic">> := Topic}})->
    {ok,Messages}=wsapp_server:get_messages(Topic),
    {json,200,#{ <<"Content-Type">> => <<"application/json">>},#{<<"topic">> => Topic ,<<"messages">> =>Messages}}.

get_subscriptions(Req=#{bindings := #{<<"user">> := User}})->
    try
        {ok,Subscriptions}=wsapp_server:get_subscriptions(User),
        {json,200,#{},#{ <<"user">> => User, <<"subscriptions">> =>Subscriptions}}
    catch
        Error:Reason -> {status,500,#{},#{<<"error">> => Error ,<<"reason">> =>Reason}}
    end.



