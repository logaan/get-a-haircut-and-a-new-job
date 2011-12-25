-module(interface).
-compile(export_all).
-include_lib("../cecho/include/cecho.hrl").
-include_lib("eunit/include/eunit.hrl").

start() ->
  setup_curses(),
  handle_input({0, 0}).

handle_input(Position) ->
  redraw_world(Position),
  case cecho:getch() of
    $k -> handle_input(move_up(Position));
    $j -> handle_input(move_down(Position));
    $h -> handle_input(move_left(Position));
    $l -> handle_input(move_right(Position));
    $ -> teardown_curses();
    _ -> handle_input(Position)
  end.

move_up({X, Y}) when Y > 0 -> {X, Y-1};
move_up(Position) -> Position.

move_down({X, Y}) when Y < 23 -> {X, Y+1};
move_down(Position) -> Position.

move_left({X, Y}) when X > 0 -> {X-1, Y};
move_left(Position) -> Position.

move_right({X, Y}) when X < 79 -> {X+1, Y};
move_right(Position) -> Position.

% IOey things. yuck.

setup_curses() ->
  application:start(cecho),
  cecho:cbreak(),
  cecho:noecho(),
  cecho:curs_set(?ceCURS_INVISIBLE).

teardown_curses() ->
  application:stop(cecho),
  erlang:halt().

redraw_world({X, Y}) ->
  cecho:erase(),
  cecho:mvaddstr(Y, X, "@"),
  cecho:refresh().

