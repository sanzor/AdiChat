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
                 {"/", { wsapp_main_controller, index}, #{methods => [get]}},
                 {"/ws/id/:id",wsapp_ws,#{protocol=>ws,idle_timeout=>30000}},
                 {"/get-user",{wsapp_main_controller,get_user},#{methods=>[get,options]}},
                 {"/get-user-by-email",{wsapp_main_controller,get_user_by_email},#{methods=>[get,options]}},
                 {"/create-user",{wsapp_main_controller,create_user},#{methods=>[post,options]}},
                 {"/delete-user",{wsapp_main_controller,delete_user},#{methods=>[delete]}},
                 {"/create-topic",{wsapp_main_controller,create_topic},#{methods=>[post]}},
                 {"/delete-topic",{wsapp_main_controller,delete_topic},#{methods=>[delete]}},
                 {"/publish",{wsapp_main_controller,publish},#{ methods =>[post]}},
                 {"/subscribe",{wsapp_main_controller,subscribe},#{ methods =>[post]}},
                 {"/unsubscribe",{wsapp_main_controller,unsubscribe},#{ methods =>[post]}},
                 {"/get_messages/topic/:topic",{wsapp_main_controller,get_messages},#{methods=>[get]}},
                 {"/get_subscriptions/user/:user",{wsapp_main_controller,get_subscriptions},#{methods=>[get]}},
                 {"/assets/[...]", "assets"}
                ]
      }].
