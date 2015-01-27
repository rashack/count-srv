-module(count_srv_sup).

-behaviour(supervisor).

-export([ start_link/0
        , init/1
        ]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Procs = [ { count_srv
            , {count_srv, start_link, []}
            , permanent
            , 2000
            , worker
            , [count_srv]
            }],
  {ok, { {one_for_one, 0, 1}
       , Procs
       }}.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
