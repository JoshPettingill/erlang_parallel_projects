%%%-------------------------------------------------------------------
%% @doc mov_fsm_ap public API
%% @end
%%%-------------------------------------------------------------------

-module(mov_fsm_ap_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    mov_fsm_ap_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
