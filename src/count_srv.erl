-module(count_srv).

-behaviour(gen_server).

-export([ start_link/0
        , inc/1
        , dec/1
        , get_term_ucount/0
        , get_term_tcount/0
        , get_term_score/0
        , get_sorted_score/0
        , get_terms/0
        , get_pos_terms/0
        , get_neg_terms/0
        , stop/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SRV_NAME, term_counter).

-record(state,
        { counter = dict:new()
        , total   = 0
        }).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

init([]) ->
  {ok, #state{}}.

handle_call(get_ucount, _From, State)     ->
  Reply = dict:size(State#state.counter),
  {reply, Reply, State};
handle_call(get_tcount, _From, State)     ->
  {reply, total, State};
handle_call(get_term_score, _From, State) ->
  Reply = get_count(State),
  {reply, Reply, State};
handle_call(get_sorted_term_score, _From, State) ->
  Reply = get_sorted_score(State),
  {reply, Reply, State};
handle_call(get_terms, _From, State)      ->
  Reply = dict:to_list(State#state.counter),
  {reply, Reply, State};
handle_call(get_pos_terms, _From, State)  ->
  Reply = get_pos_terms(State),
  {reply, Reply, State};
handle_call(get_neg_terms, _From, State)  ->
  Reply = get_neg_terms(State),
  {reply, Reply, State};
handle_call(_Req, _From, State)           ->
  {reply, ok, State}.

handle_cast({inc, Inc, Term},
            #state{counter = Terms0, total = Total} = State) ->
  Terms = dict:update_counter(Term, Inc, Terms0),
  {noreply, State#state{counter = Terms, total = Total + Inc}};
handle_cast(stop, State)                                     ->
  {stop, normal, State};
handle_cast(_Msg, State)                                     ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%=============================================================================
%%% API
%%%=============================================================================

start_link() ->
  gen_server:start_link({local, ?SRV_NAME}, ?MODULE, [], []).

dec(Term) ->
  gen_server:cast(?SRV_NAME, {inc, -1, Term}).

inc(Term) ->
  gen_server:cast(?SRV_NAME, {inc, 1, Term}).

get_term_ucount() ->
  gen_server:call(?SRV_NAME, get_ucount).

get_term_tcount() ->
  gen_server:call(?SRV_NAME, get_tcount).

get_term_score() ->
  gen_server:call(?SRV_NAME, get_term_score).

get_sorted_score() ->
  gen_server:call(?SRV_NAME, get_sorted_term_score).

get_terms() ->
  gen_server:call(?SRV_NAME, get_terms).

get_pos_terms() ->
  gen_server:call(?SRV_NAME, get_pos_terms).

get_neg_terms() ->
  gen_server:call(?SRV_NAME, get_neg_terms).

stop() ->
  gen_server:cast(?SRV_NAME, stop).

%%%=============================================================================
%%% internal
%%%=============================================================================

get_pos_terms(State) ->
  dict:to_list(dict:filter(fun(_K, C) -> C > 0 end,
                           State#state.counter)).

get_neg_terms(State) ->
  dict:to_list(dict:filter(fun(_K, C) -> C < 0 end,
                           State#state.counter)).

get_count(#state{counter = Dict}) ->
  dict:fold(fun(_K, V, Acc) -> dict:update_counter(V, 1, Acc) end,
            dict:new(), Dict).

get_sorted_score(State) ->
  Dict = get_count(State),
  Count = lists:sort(dict:to_list(Dict)),
  [ {n_keys, dict:size(Dict)}
  , {count,  Count}
  , {total, State#state.total}
  ].

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
