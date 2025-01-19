-module(wsapp_router).
-behaviour(nova_router).

-export([
         routes/1
        ]).

%% The Environment-variable is defined in your sys.config in {nova, [{environment, Value}]}
routes(_Environment) ->
    [#{prefix => "",
      security => false,
      routes => [
                %  {"/", { wsapp_main_controller, index}, #{methods => [get]}},
                 {"/ws/id/:id",wsapp_ws,#{protocol=>ws,idle_timeout=>30000}},
                 {"/get-user",{user_controller,get_user},#{methods=>[get,options]}},
                 {"/get-user-by-email",{user_controller,get_user_by_email},#{methods=>[get,options]}},
                 {"/create-user",{user_controller,create_user},#{methods=>[post,options]}},
                 {"/delete-user",{user_controller,delete_user},#{methods=>[delete]}},
                 
                 {"/create-topic",{topic_controller,create_topic},#{methods=>[post]}},
                 {"/delete-topic",{topic_controller,delete_topic},#{methods=>[delete]}},
                 {"/subscribe",{topic_controller,subscribe},#{ methods =>[post,options]}},
                 {"/unsubscribe",{topic_controller,unsubscribe},#{ methods =>[post]}},
                 {"/get_subscriptions/:user_id",{topic_controller,get_subscriptions},#{methods=>[get]}},

                 {"/publish",{message_controller,publish},#{ methods =>[post]}},
                 {"/get_messages/topic/:topic",{message_controller,get_messages},#{methods=>[get]}},
                
                 {"/assets/[...]", "assets"}
                ]
      }].
