-module(debug_controller).
-export([
    get_table/1
]).


get_table(#{bindings := #{ table :=TableBin }})->
    Table=binary_to_atom(TableBin),
    Result=wsapp_server:get_messages(Table),
    {json,200,#{},#{<<"table">> => Table, <<"messages">> =>Result }}.
    