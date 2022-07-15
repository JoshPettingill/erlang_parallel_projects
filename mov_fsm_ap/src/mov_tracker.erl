-module(mov_tracker).
-export([start_link/0, add/2, delete/2, stop/0, move_to_origin/0]).
-export([moved_to/1, no_change/0]).

% API functions
start_link() ->
    case gen_event:start_link({local, ?MODULE}) of
	{ok, Pid} ->
	    add(mov_logger, {file, "movement.log"}),
        add(mov_logger, standard_io),
	    {ok, Pid};
	Error ->
	    Error
    end.

stop() -> 
    io:format("Test: Deleting Log file connection~n"),
    delete(mov_logger, {file, "movement.log"}),
    io:format("Test: Deleting Log io connection~n"),
    delete(mov_logger,standard_io),
    io:format("Test: Deleted connections~n").
    % gen_event:stop({local, ?MODULE}).

moved_to(Location) -> gen_event:notify(?MODULE, {event, {moved_to, Location}}).
no_change() -> gen_event:notify(?MODULE, {event, no_change}).
move_to_origin() -> gen_event:notify(?MODULE, {event, {started_at, {0,0}}}).

add(M,A) -> gen_event:add_handler(?MODULE, M, A).
delete(M,A) -> gen_event:delete_handler(?MODULE,M,A).