-module(movement_fsm_test).
-include_lib("eunit/include/eunit.hrl").
-import(movement_fsm, [start/0, stop/0, 
                    stop_moving/0, start_moving_left/0, start_moving_right/0, 
                    start_moving_down/0, start_moving_up/0, keep_moving/0]).
-import(mov_tracker, [moved_to/1]).
-export([]).

%% test that the fsm start_link function returns the correct values
start_fsm_test() ->
    {ok,_} = movement_fsm:start_link().

%% test that fsm stop function returns the correct values
stop_fsm_test() ->
    ok = movement_fsm:stop().

%% Test each user function to ensure that it sends events to the fsm
basic_moving_commands_send_correct_events_test_() ->
    %% Fixture for setting up stuff before the test
    {setup,
    %% setup
    fun() -> meck:new(mov_tracker, [unstick, stub_all]),
             gen_fsm:start_link({local, movement_fsm}, movement_fsm, [], [])
        end,
    %% cleanup
    fun(_) -> meck:unload(mov_tracker),
              gen_fsm:stop(movement_fsm)
            end,
    %% tests
    [fun()-> ?assertMatch(ok, movement_fsm:stop_moving()) end,
    fun()-> ?assertMatch(ok, movement_fsm:start_moving_left()) end,
    fun()-> ?assertMatch(ok, movement_fsm:start_moving_right()) end,
    fun()-> ?assertMatch(ok, movement_fsm:start_moving_down()) end,
    fun()-> ?assertMatch(ok, movement_fsm:start_moving_up()) end,
    fun()-> ?assertMatch(ok, movement_fsm:keep_moving()) end
    ]}.

%% test that the correct state transition function is called when starting
%% in the not_moving state.
not_moving_state_test_() ->
    %% test fixture for doing setup/cleanup for each test individually
    {foreach,
    %% setup
    fun() -> meck:new(mov_tracker, [unstick, stub_all]),
             meck:new(movement_fsm, [non_strict, passthrough]),
             gen_fsm:start_link({local, movement_fsm}, movement_fsm, [], [])
             end,
    %% cleanup
    fun(_) -> meck:unload(mov_tracker),
              meck:unload(movement_fsm),
              gen_fsm:stop(movement_fsm)
              end,
    %% tests
    [fun()-> meck:expect(movement_fsm, not_moving, fun(stop_moving, {0,0}) -> {next_state, not_moving, {0,0}} end),
             ?assertMatch(ok, movement_fsm:stop_moving()),
             meck:validate(movement_fsm) end,
    fun()-> meck:expect(movement_fsm, not_moving, fun(start_moving_right, {0,0}) -> {next_state, moving, {{5,0}, {5,0}}} end),
            ?assertMatch(ok, movement_fsm:start_moving_right()),
            meck:validate(movement_fsm) end,
    fun()-> meck:expect(movement_fsm, not_moving, fun(start_moving_left, {0,0}) -> {next_state, moving, {{-5,0}, {-5,0}}} end),
            ?assertMatch(ok, movement_fsm:start_moving_left()),
            meck:validate(movement_fsm) end,
    fun()-> meck:expect(movement_fsm, not_moving, fun(start_moving_down, {0,0}) -> {next_state, moving, {{0,-5}, {0,-5}}} end),
            ?assertMatch(ok, movement_fsm:start_moving_down()),
            meck:validate(movement_fsm) end,
    fun()-> meck:expect(movement_fsm, not_moving, fun(start_moving_up, {0,0}) -> {next_state, moving, {{0,5}, {0,5}}} end),
            ?assertMatch(ok, movement_fsm:start_moving_up()),
            meck:validate(movement_fsm) end,
    fun()-> meck:expect(movement_fsm, not_moving, fun(keep_moving, {0,0}) -> {next_state, not_moving, {0,0}} end),
            ?assertMatch(ok, movement_fsm:keep_moving()),
            meck:validate(movement_fsm) end
    ]}.

%% test that the correct state transition function is called when starting
%% in the moving state, having just started moving left.
start_moving_left_moving_state_test_() ->
    %% test fixture for doing setup/cleanup for each test individually
    {foreach,
    %% setup
    fun() -> meck:new(mov_tracker, [unstick, stub_all]),
             meck:new(movement_fsm, [non_strict, passthrough]),
             gen_fsm:start_link({local, movement_fsm}, movement_fsm, [], []),
             ?assertMatch(ok, movement_fsm:start_moving_left())
             end,
    %% cleanup
    fun(_) -> meck:unload(mov_tracker),
              meck:unload(movement_fsm),
              gen_fsm:stop(movement_fsm)
              end,
    %% tests
    [fun()-> meck:expect(movement_fsm, moving, fun(stop_moving, {{-5,0},{-5,0}}) -> {next_state, not_moving, {-5,0}} end),
             ?assertMatch(ok, movement_fsm:stop_moving()),
             meck:validate(movement_fsm) end,
    fun()-> meck:expect(movement_fsm, moving, fun(start_moving_right, {{-5,0},{-5,0}}) -> {next_state, moving, {{0,0}, {5,0}}} end),
            ?assertMatch(ok, movement_fsm:start_moving_right()),
            meck:validate(movement_fsm) end,
    fun()-> meck:expect(movement_fsm, moving, fun(start_moving_left, {{-5,0},{-5,0}}) -> {next_state, moving, {{-10,0}, {-5,0}}} end),
            ?assertMatch(ok, movement_fsm:start_moving_left()),
            meck:validate(movement_fsm) end,
    fun()-> meck:expect(movement_fsm, moving, fun(start_moving_down, {{-5,0},{-5,0}}) -> {next_state, moving, {{-5,-5}, {-5,-5}}} end),
            ?assertMatch(ok, movement_fsm:start_moving_down()),
            meck:validate(movement_fsm) end,
    fun()-> meck:expect(movement_fsm, moving, fun(start_moving_up, {{-5,0},{-5,0}}) -> {next_state, moving, {{-5,5}, {-5,5}}} end),
            ?assertMatch(ok, movement_fsm:start_moving_up()),
            meck:validate(movement_fsm) end,
    fun()-> meck:expect(movement_fsm, moving, fun(keep_moving, {{-5,0},{-5,0}}) -> {next_state, moving, {-10,0}} end),
            ?assertMatch(ok, movement_fsm:keep_moving()),
            meck:validate(movement_fsm) end
    ]}.

%% test that the correct state transition function is called when starting
%% in the moving state, having just started moving right.
start_moving_right_moving_state_test_() ->
    %% test fixture for doing setup/cleanup for each test individually
    {foreach,
    %% setup
    fun() -> meck:new(mov_tracker, [unstick, stub_all]),
             meck:new(movement_fsm, [non_strict, passthrough]),
             gen_fsm:start_link({local, movement_fsm}, movement_fsm, [], []),
             ?assertMatch(ok, movement_fsm:start_moving_right())
             end,
    %% cleanup
    fun(_) -> meck:unload(mov_tracker),
              meck:unload(movement_fsm),
              gen_fsm:stop(movement_fsm)
              end,
    %% tests
    [fun()-> meck:expect(movement_fsm, moving, fun(stop_moving, {{5,0},{5,0}}) -> {next_state, not_moving, {5,0}} end),
             ?assertMatch(ok, movement_fsm:stop_moving()),
             meck:validate(movement_fsm) end,
    fun()-> meck:expect(movement_fsm, moving, fun(start_moving_right, {{5,0},{5,0}}) -> {next_state, moving, {{10,0}, {5,0}}} end),
            ?assertMatch(ok, movement_fsm:start_moving_right()),
            meck:validate(movement_fsm) end,
    fun()-> meck:expect(movement_fsm, moving, fun(start_moving_left, {{5,0},{5,0}}) -> {next_state, moving, {{0,0}, {-5,0}}} end),
            ?assertMatch(ok, movement_fsm:start_moving_left()),
            meck:validate(movement_fsm) end,
    fun()-> meck:expect(movement_fsm, moving, fun(start_moving_down, {{5,0},{5,0}}) -> {next_state, moving, {{5,-5}, {5,-5}}} end),
            ?assertMatch(ok, movement_fsm:start_moving_down()),
            meck:validate(movement_fsm) end,
    fun()-> meck:expect(movement_fsm, moving, fun(start_moving_up, {{5,0},{5,0}}) -> {next_state, moving, {{5,5}, {5,5}}} end),
            ?assertMatch(ok, movement_fsm:start_moving_up()),
            meck:validate(movement_fsm) end,
    fun()-> meck:expect(movement_fsm, moving, fun(keep_moving, {{5,0},{5,0}}) -> {next_state, moving, {10,0}} end),
            ?assertMatch(ok, movement_fsm:keep_moving()),
            meck:validate(movement_fsm) end
    ]}.

%% test that the correct state transition function is called when starting
%% in the moving state, having just started moving down.
start_moving_down_moving_state_test_() ->
    %% test fixture for doing setup/cleanup for each test individually
    {foreach,
    %% setup
    fun() -> meck:new(mov_tracker, [unstick, stub_all]),
             meck:new(movement_fsm, [non_strict, passthrough]),
             gen_fsm:start_link({local, movement_fsm}, movement_fsm, [], []),
             ?assertMatch(ok, movement_fsm:start_moving_down())
             end,
    %% cleanup
    fun(_) -> meck:unload(mov_tracker),
              meck:unload(movement_fsm),
              gen_fsm:stop(movement_fsm)
              end,
    %% tests
    [fun()-> meck:expect(movement_fsm, moving, fun(stop_moving, {{0,-5},{0,-5}}) -> {next_state, not_moving, {0,-5}} end),
             ?assertMatch(ok, movement_fsm:stop_moving()),
             meck:validate(movement_fsm) end,
    fun()-> meck:expect(movement_fsm, moving, fun(start_moving_right, {{0,-5},{0,-5}}) -> {next_state, moving, {{5,-5}, {5,-5}}} end),
            ?assertMatch(ok, movement_fsm:start_moving_right()),
            meck:validate(movement_fsm) end,
    fun()-> meck:expect(movement_fsm, moving, fun(start_moving_left, {{0,-5},{0,-5}}) -> {next_state, moving, {{-5,-5}, {-5,-5}}} end),
            ?assertMatch(ok, movement_fsm:start_moving_left()),
            meck:validate(movement_fsm) end,
    fun()-> meck:expect(movement_fsm, moving, fun(start_moving_down, {{0,-5},{0,-5}}) -> {next_state, moving, {{0,-10}, {0,-5}}} end),
            ?assertMatch(ok, movement_fsm:start_moving_down()),
            meck:validate(movement_fsm) end,
    fun()-> meck:expect(movement_fsm, moving, fun(start_moving_up, {{0,-5},{0,-5}}) -> {next_state, moving, {{0,0}, {0,5}}} end),
            ?assertMatch(ok, movement_fsm:start_moving_up()),
            meck:validate(movement_fsm) end,
    fun()-> meck:expect(movement_fsm, moving, fun(keep_moving, {{0,-5},{0,-5}}) -> {next_state, moving, {0,-10}} end),
            ?assertMatch(ok, movement_fsm:keep_moving()),
            meck:validate(movement_fsm) end
    ]}.

%% test that the correct state transition function is called when starting
%% in the moving state, having just started moving up.
start_moving_up_moving_state_test_() ->
    %% test fixture for doing setup/cleanup for each test individually
    {foreach,
    %% setup
    fun() -> meck:new(mov_tracker, [unstick, stub_all]),
             meck:new(movement_fsm, [non_strict, passthrough]),
             gen_fsm:start_link({local, movement_fsm}, movement_fsm, [], []),
             ?assertMatch(ok, movement_fsm:start_moving_up())
             end,
    %% cleanup
    fun(_) -> meck:unload(mov_tracker),
              meck:unload(movement_fsm),
              gen_fsm:stop(movement_fsm)
              end,
    %% tests
    [fun()-> meck:expect(movement_fsm, moving, fun(stop_moving, {{0,5},{0,5}}) -> {next_state, not_moving, {0,5}} end),
             ?assertMatch(ok, movement_fsm:stop_moving()),
             meck:validate(movement_fsm) end,
    fun()-> meck:expect(movement_fsm, moving, fun(start_moving_right, {{0,5},{0,5}}) -> {next_state, moving, {{5,5}, {5,5}}} end),
            ?assertMatch(ok, movement_fsm:start_moving_right()),
            meck:validate(movement_fsm) end,
    fun()-> meck:expect(movement_fsm, moving, fun(start_moving_left, {{0,5},{0,5}}) -> {next_state, moving, {{-5,5}, {-5,5}}} end),
            ?assertMatch(ok, movement_fsm:start_moving_left()),
            meck:validate(movement_fsm) end,
    fun()-> meck:expect(movement_fsm, moving, fun(start_moving_down, {{0,5},{0,5}}) -> {next_state, moving, {{0,0}, {0,-5}}} end),
            ?assertMatch(ok, movement_fsm:start_moving_down()),
            meck:validate(movement_fsm) end,
    fun()-> meck:expect(movement_fsm, moving, fun(start_moving_up, {{0,5},{0,5}}) -> {next_state, moving, {{0,10}, {0,5}}} end),
            ?assertMatch(ok, movement_fsm:start_moving_up()),
            meck:validate(movement_fsm) end,
    fun()-> meck:expect(movement_fsm, moving, fun(keep_moving, {{0,5},{0,5}}) -> {next_state, moving, {0,10}} end),
            ?assertMatch(ok, movement_fsm:keep_moving()),
            meck:validate(movement_fsm) end
    ]}.

%% test that the finite state machine is in the correct "location" after
%% each movmenent function is called.
do_some_movement_test_() ->
    %% test fixture for doing setup/cleanup for each test individually
    {setup,
    %% setup
    fun() -> meck:new(mov_tracker, [unstick, stub_all]),
             meck:new(movement_fsm, [non_strict, passthrough]),
             gen_fsm:start_link({local, movement_fsm}, movement_fsm, [], [])
             end,
    %% cleanup
    fun(_) -> meck:unload(mov_tracker),
              meck:unload(movement_fsm),
              gen_fsm:stop(movement_fsm)
              end,
    %% test that the fsm correctly passes the data from one state to the next, and simulates movement correctly.
    [fun()-> meck:expect(movement_fsm, moving, fun(start_moving_left, {0,0}) -> {next_state, moving, {{-5,0},{-5,0}}} end),
             ?assertMatch(ok, movement_fsm:start_moving_left()),
             meck:validate(movement_fsm) end,
    fun()-> meck:expect(movement_fsm, moving, fun(keep_moving, {{-5,0},{-5,0}}) -> {next_state, moving, {{-10,0}, {-5,0}}} end),
             ?assertMatch(ok, movement_fsm:keep_moving()),
             meck:validate(movement_fsm) end,
    fun()->  meck:expect(movement_fsm, moving, fun(start_moving_left, {{-10,0},{-5,0}}) -> {next_state, moving, {{-15,0}, {-5,0}}} end),
             ?assertMatch(ok, movement_fsm:start_moving_left()),
             meck:validate(movement_fsm) end,
    fun()->  meck:expect(movement_fsm, moving, fun(start_moving_right, {{-15,0},{-5,0}}) -> {next_state, moving, {{-10,0}, {5,0}}} end),
             ?assertMatch(ok, movement_fsm:start_moving_right()),
             meck:validate(movement_fsm) end,
    fun()->  meck:expect(movement_fsm, moving, fun(start_moving_up, {{-10,0},{5,0}}) -> {next_state, moving, {{-5,5}, {5,5}}} end),
             ?assertMatch(ok, movement_fsm:start_moving_up()),
             meck:validate(movement_fsm) end,
    fun()->  meck:expect(movement_fsm, moving, fun(start_moving_down, {{-5,5},{5,5}}) -> {next_state, moving, {{-5,0}, {5,-5}}} end),
             ?assertMatch(ok, movement_fsm:start_moving_down()),
             meck:validate(movement_fsm) end,
    fun()->  meck:expect(movement_fsm, moving, fun(stop_moving, {{-5,0},{5,-5}}) -> {next_state, moving, {-5,0}} end),
             ?assertMatch(ok, movement_fsm:stop_moving()),
             meck:validate(movement_fsm) end
    ]}.
