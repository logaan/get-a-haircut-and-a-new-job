-module(interface).
-compile(export_all).
-include_lib("../cecho/include/cecho.hrl").
-include_lib("eunit/include/eunit.hrl").

start() ->
  setup(),
  start_game().

start_game() ->
  StartingPosition = {0, 0},
  print_string(StartingPosition, "@"),
  cecho:refresh(),
  loop(StartingPosition).

loop(OldPosition) ->
  case cecho:getch() of
    $k -> redraw_player(OldPosition, move_up(OldPosition));
    $j -> redraw_player(OldPosition, move_down(OldPosition));
    $h -> redraw_player(OldPosition, move_left(OldPosition));
    $l -> redraw_player(OldPosition, move_right(OldPosition));
    $ -> teardown();
    _ -> loop(OldPosition)
  end.

move_up({X, Y})    -> {X, Y-1}.
move_down({X, Y})  -> {X, Y+1}.
move_left({X, Y})  -> {X-1, Y}.
move_right({X, Y}) -> {X+1, Y}.

redraw_player(OldPosition, NewPosition) ->
  print_string(OldPosition, " "),
  print_string(NewPosition, "@"),
  cecho:refresh(),
  loop(NewPosition).

print_string({X, Y}, String) ->
  cecho:mvaddstr(Y, X, String).

setup() ->
  application:start(cecho),
  cecho:cbreak(),
  cecho:noecho(),
  cecho:curs_set(?ceCURS_INVISIBLE).

teardown() ->
  application:stop(cecho),
  erlang:halt().

