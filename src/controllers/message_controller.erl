-module(message_controller).
-export([
         index/1,
         publish/1,
         get_messages/1
        ]).

index(_Req) ->
    {ok, [{message, "Hello world!"}]}.


publish(_Req=#{json := #{ <<"topic">> := Topic , <<"sender">> := _Sender , <<"message">> := Message}})->
    ok=wsapp_server:publish(Topic,Message),
    {status,200}.

get_messages(_Req=#{parsed_qs := #{ <<"topic">> := Topic}})->
    {ok,Messages}=wsapp_server:get_messages(Topic),
    {json,200,#{ <<"Content-Type">> => <<"application/json">>},#{<<"topic">> => Topic ,<<"messages">> =>Messages}}.


