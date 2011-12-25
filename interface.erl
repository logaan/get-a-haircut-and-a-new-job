-module(interface).
-compile(export_all).
-include_lib("../cecho/include/cecho.hrl").
-include_lib("eunit/include/eunit.hrl").

start() ->
  setup_curses(),
  game_loop({0, 0}, [{2, 0, "D"},
                     {2, 2, "B"},
                     {2, 4, "H"},
                     {2, 6, "G"},
                     {2, 8, "M"}]).

game_loop(Position, NPCs) ->
  redraw_world(Position, NPCs),
  case cecho:getch() of
    $ -> teardown_curses();
    $k -> game_loop(move_up(Position), NPCs);
    $j -> game_loop(move_down(Position), NPCs);
    $h -> game_loop(move_left(Position), NPCs);
    $l -> game_loop(move_right(Position), NPCs);
    _ -> game_loop(Position, NPCs)
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

redraw_world(Player, NPCs) ->
  cecho:erase(),
  draw_npcs(NPCs),
  draw_player(Player),
  cecho:refresh().

draw_player({X, Y}) ->
  cecho:mvaddstr(Y, X, "@").

draw_npcs([]) -> ok;
draw_npcs([{X, Y, Char} | NPCs]) ->
  cecho:mvaddstr(Y, X, Char),
  draw_npcs(NPCs).

