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
                 {"/ws/:user",wsapp_ws,#{protocol=>ws,idle_timeout=>30000}},
                 {"/publish",{wsapp_main_controller,publish},#{ methods =>[post]}},
                 {"/subscribe",{wsapp_main_controller,subscribe},#{ methods =>[post]}},
                 {"/unsubscribe",{wsapp_main_controller,unsubscribe},#{ methods =>[post]}},
                 {"/assets/[...]", "assets"}
                ]
      }].
