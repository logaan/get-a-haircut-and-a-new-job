-module(interface).
-compile(export_all).
-include_lib("../cecho/include/cecho.hrl").
-include_lib("eunit/include/eunit.hrl").

start() ->
  setup_curses(),

  StartingPosition = {0, 0},
  print_string(StartingPosition, "@"),
  cecho:refresh(),

  handle_input(StartingPosition).

setup_curses() ->
  application:start(cecho),
  cecho:cbreak(),
  cecho:noecho(),
  cecho:curs_set(?ceCURS_INVISIBLE).

teardown_curses() ->
  application:stop(cecho),
  erlang:halt().

handle_input(OldPosition) ->
  case cecho:getch() of
    $k -> redraw_player(OldPosition, move_up(OldPosition));
    $j -> redraw_player(OldPosition, move_down(OldPosition));
    $h -> redraw_player(OldPosition, move_left(OldPosition));
    $l -> redraw_player(OldPosition, move_right(OldPosition));
    $ -> teardown_curses();
    _ -> handle_input(OldPosition)
  end.

redraw_player(OldPosition, NewPosition) ->
  print_string(OldPosition, " "),
  print_string(NewPosition, "@"),
  cecho:refresh(),
  handle_input(NewPosition).

print_string({X, Y}, String) ->
  cecho:mvaddstr(Y, X, String).

move_up({X, Y}) when Y > 0 -> {X, Y-1};
move_up(Position) -> Position.

move_down({X, Y}) when Y < 23 -> {X, Y+1};
move_down(Position) -> Position.

move_left({X, Y}) when X > 0 -> {X-1, Y};
move_left(Position) -> Position.

move_right({X, Y}) when X < 79 -> {X+1, Y};
move_right(Position) -> Position.

