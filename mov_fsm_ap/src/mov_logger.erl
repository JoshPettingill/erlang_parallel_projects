-module(mov_logger).
-behaviour(gen_event).

-export([handle_info/2, handle_event/2, init/1, terminate/2]).

init(standard_io)  ->
    {ok, {standard_io, 1}};
init({file, File}) ->
    {ok, Fd} = file:open(File, write),
    {ok, {Fd, 1}};
init(Args) ->
    {error, {args, Args}}.

terminate(_Reason, {standard_io, Count}) ->
    {count, Count};
terminate(_Reason, {Fd, Count}) ->
    file:close(Fd),
    {count, Count}.

handle_event(Event, {Fd, Count}) ->
    print(Fd, Count, Event, "Event"),
    {ok, {Fd, Count+1}}.

handle_info(Event, {Fd, Count}) ->
    print(Fd, Count, Event, "Unknown"),
    {ok, {Fd, Count+1}}.

print(Fd, Count, Event, Tag) ->
    io:format(Fd, "Id:~w Time:~w Date:~w~n~s:~w~n",
              [Count, time(),date(),Tag,Event]).