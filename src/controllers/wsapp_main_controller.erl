-module(wsapp_main_controller).
-export([
         index/1,
         publish/1,
         subscribe/1,
         unsubscribe/1
        ]).

index(_Req) ->
    {ok, [{message, "Hello world!"}]}.

publish(_Req=#{parsed_qs := #{ <<"topic">> := Topic , <<"sender">> := _Sender , <<"message">> := Message}})->
    ok=wsapp_server:publish(Topic,Message),
    {status,200}.

subscribe(_Req=#{parsed_qs := #{ <<"user">> :=User, <<"topic">> := Topic}})->
    ok=wsapp_server:subscribe(User,Topic),
    {status,200}.

unsubscribe(_Req=#{parsed_qs := #{ <<"user">> :=User, <<"topic">> := Topic}})->
    ok=wsapp_server:unsubscribe(User,Topic),
    {status,200}.

get_messages(Req=#{parsed_qs := #{ <<"topic">> := Topic}})->
    Messages=wsapp_server:get_messages(Topic),
    {json,200,#{ <<"Content-Type">> => <<"application/json">>},#{  <<"topic">> => Topic ,<<"messages">> =>Messages}}.



