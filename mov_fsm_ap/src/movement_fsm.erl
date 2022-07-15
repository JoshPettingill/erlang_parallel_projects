-module(movement_fsm).

-behaviour(gen_fsm).
-export([start_link/0, stop/0]).
% Callback functions
-export([init/1, terminate/3, handle_event/3, handle_sync_event/4]).
% States
-export([not_moving/2, moving/2]).
% User functions
-export([stop_moving/0, start_moving_left/0, start_moving_right/0]).
-export([start_moving_down/0, start_moving_up/0, keep_moving/0]).


% gen_fsm functions
init([]) ->
    X = 0,
    Y = 0,
    % io:format("Test: starting event handler~n"),
    % mov_tracker:start_link(),
    % io:format("Test: started event handler~n"),
    % io:format("You are at (~p,~p)~n", [X, Y]),
    % io:format("setting tracker start position.~n", []),
    % mov_tracker:move_to_origin(),
    process_flag(trap_exit, true),
    {ok, not_moving, {X, Y}}.

handle_event(stop, _State, LoopData) ->
    {stop, normal, LoopData}.

handle_sync_event(stop, _From, _State, LoopData) ->
    {stop, normal, LoopData}.

terminate(_Reason, _StateName, _LoopData) ->
    % io:format("Test: Stopping event handler~n"),
    % mov_tracker:stop(),
    % io:format("Test: Stopping fsm~n"),
    ok.

% State functions
not_moving(start_moving_left, {PositionX, PositionY}) ->
    Next_Loc = {PositionX - 5, PositionY},
    mov_tracker:moved_to(Next_Loc),
    io:format("You are at (~p,~p) and have started moving left.~n", [PositionX - 5, PositionY]),
    {next_state, moving, {Next_Loc, {-5, 0}}};
not_moving(start_moving_right, {PositionX, PositionY}) ->
    Next_Loc = {PositionX + 5, PositionY},
    mov_tracker:moved_to(Next_Loc),
    io:format("You are at (~p,~p) and have started moving right.~n", [PositionX + 5, PositionY]),
    {next_state, moving, {Next_Loc, {5, 0}}};
not_moving(start_moving_up, {PositionX, PositionY}) ->
    Next_Loc = {PositionX, PositionY + 5},
    mov_tracker:moved_to(Next_Loc),
    io:format("You are at (~p,~p) and have started moving up.~n", [PositionX, PositionY + 5]),
    {next_state, moving, {Next_Loc, {0, 5}}};
not_moving(start_moving_down, {PositionX, PositionY}) ->
    Next_Loc = {PositionX, PositionY - 5},
    mov_tracker:moved_to(Next_Loc),
    io:format("You are at (~p,~p) and have started moving down.~n", [PositionX, PositionY - 5]),
    {next_state, moving, {Next_Loc, {0, -5}}};
not_moving(_Other, Positions) ->
    % io:format("Other:~p ~p~n",[Other, Positions]),
    io:format("You cannot do that yet.~n"),
    {next_state, not_moving, {Positions}}.

moving(stop_moving, {{PositionX, PositionY}, _}) ->
    io:format("Stopped moving~n\tYou are Position: (~p,~p)~n", [PositionX, PositionY]),
    mov_tracker:no_change(),
    {next_state, not_moving, {PositionX, PositionY}};
moving(start_moving_left, {{PositionX, PositionY}, {VelocityX, VelocityY}}) when VelocityX >= 0 ->
    NewVelocityX = -5,
    NewPositionX = PositionX + NewVelocityX,
    NewPositionY = PositionY + VelocityY,
    mov_tracker:moved_to({NewPositionX, NewPositionY}),
    io:format("Started to move left~n"),
    io:format("\tYou are at Position:(~p,~p) w/ Velocity: (~p,~p)~n", [
        NewPositionX, NewPositionY, NewVelocityX, VelocityY
    ]),
    {next_state, moving, {{NewPositionX, NewPositionY}, {NewVelocityX, VelocityY}}};
moving(start_moving_right, {{PositionX, PositionY}, {VelocityX, VelocityY}}) when VelocityX =< 0 ->
    NewVelocityX = 5,
    NewPositionX = PositionX + NewVelocityX,
    NewPositionY = PositionY + VelocityY,
    mov_tracker:moved_to({NewPositionX, NewPositionY}),
    io:format("Started to move right~n"),
    io:format("\tYou are at Position:(~p,~p) w/ Velocity: (~p,~p)~n", [
        NewPositionX, NewPositionY, NewVelocityX, VelocityY
    ]),
    {next_state, moving, {{NewPositionX, NewPositionY}, {NewVelocityX, VelocityY}}};
moving(start_moving_up, {{PositionX, PositionY}, {VelocityX, VelocityY}}) when VelocityY =< 0 ->
    NewVelocityY = 5,
    NewPositionX = PositionX + VelocityX,
    NewPositionY = PositionY + NewVelocityY,
    mov_tracker:moved_to({NewPositionX, NewPositionY}),
    io:format("Started to move up~n"),
    io:format("\tYou are at Position:(~p,~p) w/ Velocity: (~p,~p)~n", [
        NewPositionX, NewPositionY, VelocityX, NewVelocityY
    ]),
    {next_state, moving, {{NewPositionX, NewPositionY}, {VelocityX, NewVelocityY}}};
moving(start_moving_down, {{PositionX, PositionY}, {VelocityX, VelocityY}}) when VelocityY >= 0 ->
    NewVelocityY = -5,
    NewPositionX = PositionX + VelocityX,
    NewPositionY = PositionY + NewVelocityY,
    mov_tracker:moved_to({NewPositionX, NewPositionY}),
    io:format("Started to move down~n"),
    io:format("\tYou are at Position:(~p,~p) w/ Velocity: (~p,~p)~n", [
        NewPositionX, NewPositionY, VelocityX, NewVelocityY
    ]),
    {next_state, moving, {{NewPositionX, NewPositionY}, {VelocityX, NewVelocityY}}};
moving(_Other, {{PositionX, PositionY}, {VelocityX, VelocityY}}) ->
    NewPositionX = PositionX + VelocityX,
    NewPositionY = PositionY + VelocityY,
    mov_tracker:moved_to({NewPositionX, NewPositionY}),
    io:format("Kept moving~n"),
    io:format("\tYou are at Position:(~p,~p) w/ Velocity: (~p,~p)~n", [
        NewPositionX, NewPositionY, VelocityX, VelocityY
    ]),
    {next_state, moving, {{NewPositionX, NewPositionY}, {VelocityX, VelocityY}}}.

% User functions
start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> 
    gen_fsm:send_all_state_event(?MODULE, stop).

stop_moving() ->
    gen_fsm:send_event(?MODULE, stop_moving).

start_moving_left() ->
    % io:format("about to send start_moving_left:~n", []),
    gen_fsm:send_event(?MODULE, start_moving_left).

start_moving_right() ->
    gen_fsm:send_event(?MODULE, start_moving_right).

start_moving_up() -> 
    gen_fsm:send_event(?MODULE, start_moving_up).

start_moving_down() -> 
    gen_fsm:send_event(?MODULE, start_moving_down).

keep_moving() -> 
    gen_fsm:send_event(?MODULE, keep_moving).

    
