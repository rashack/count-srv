-module(count_srv_app).

-behaviour(application).

-export([ start/2
        , stop/1
        ]).

start(_Type, _Args) ->
  count_srv_sup:start_link().

stop(_State) ->
  ok.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
