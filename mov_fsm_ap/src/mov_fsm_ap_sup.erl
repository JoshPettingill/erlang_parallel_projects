-module(mov_fsm_ap_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([stop/0]).

start_link() ->
    supervisor:start_link({local,?MODULE},?MODULE, []).

stop() -> 
    exit(whereis(?MODULE), shutdown).

init(_) ->
    ChildSpecList = [child(mov_tracker), child(movement_fsm)],
    SupFlags = #{strategy => rest_for_one,
                 intensity => 2, 
                 period => 3600},
    {ok, {SupFlags, ChildSpecList}}.

child(Module) ->
    #{id => Module,
      start => {Module, start_link, []},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => [Module]}.