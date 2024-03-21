-module(topic_controller).
-include("../../include/domain.hrl").
-export([
         index/1,
         create_topic/1,
         delete_topic/1,
         subscribe/1,
         unsubscribe/1
        ]).

index(_Req) ->
    {ok, [{message, "Hello world!"}]}.

create_topic(_Req=#{json := #{<<"user_id">> := UserId,<<"name">> := TopicName}})->
    Params=#create_topic_params{user_id = UserId,name =TopicName},
    case wsapp_server:create_topic(Params) of
        {ok,Topic} -> T=utils:from_topic(Topic),
                      { json,200,#{<<"Content-Type">>=> <<"application/json">>},T};
        {error,{bad_request,ValidationErrors}}->
            {json,400,#{<<"Content-Type">>=> <<"application/json">>},ValidationErrors};
        {error,Error}->
            {json,500,#{<<"Content-Type">>=> <<"application/json">>},Error}
end.
        
delete_topic(_Req=#{parsed_qs:=#{id := TopicId}})->
    case wsapp_server:delete_topic(binary_to_integer(TopicId)) of
        ok -> {status,200};
        {error,Error}->
            {json,500,#{<<"Content-Type">>=> <<"application/json">>},Error}
end.  

subscribe(_Req=#{parsed_qs := #{ <<"userId">> :=UserId, <<"topic">> := Topic}})->
    {ok,TopicId}=wsapp_server:subscribe(UserId,Topic),
    {json,200,#{},#{<<"topicId">> => TopicId , <<"userId">> => UserId}}.

unsubscribe(_Req=#{parsed_qs := #{ <<"user">> :=User, <<"topic">> := Topic}})->
    ok=wsapp_server:unsubscribe(User,Topic),
    {status,200}.

get_subscriptions(Req=#{bindings := #{<<"user">> := User}})->
    try
        {ok,Subscriptions}=wsapp_server:get_subscriptions(User),
        {json,200,#{},#{ <<"user">> => User, <<"subscriptions">> =>Subscriptions}}
    catch
        Error:Reason -> {status,500,#{},#{<<"error">> => Error ,<<"reason">> =>Reason}}
    end.


