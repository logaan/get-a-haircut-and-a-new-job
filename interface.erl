-module(interface).
-compile(export_all).
-include_lib("../cecho/include/cecho.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(quit,  $).
-define(north, $k).
-define(south, $j).
-define(west,  $h).
-define(east,  $l).

start() ->
  setup_curses(),
  game_loop({{0, 0}, [{2, 0, "D"},
                      {2, 2, "B"},
                      {2, 4, "H"},
                      {2, 6, "G"},
                      {2, 8, "M"}]}).

game_loop(World) ->
  redraw_world(World),
  case cecho:getch() of
    ?quit -> teardown_curses();
    Direction -> game_loop(move(World, Direction))
  end.

setup_curses() ->
  application:start(cecho),
  cecho:cbreak(),
  cecho:noecho(),
  cecho:curs_set(?ceCURS_INVISIBLE).

teardown_curses() ->
  application:stop(cecho),
  erlang:halt().

redraw_world({Player, NPCs}) ->
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

% Utilities

move({OldPosition = {X, Y}, NPCs}, Direction) ->
  NewPosition = {move_x(X, Direction), move_y(Y, Direction)},
  {colision(OldPosition, NewPosition, NPCs), NPCs}.

move_y(Y, ?north) when Y > 0  -> Y - 1;
move_y(Y, ?south) when Y < 23 -> Y + 1;
move_y(Y, _) -> Y.

move_x(X, ?west) when X > 0  -> X - 1;
move_x(X, ?east) when X < 79 -> X + 1;
move_x(X, _) -> X.

colision(_OldPosition, NewPosition, []) ->
  NewPosition;
colision(OldPosition, {X, Y}, [{X, Y, _} | _NPCs]) ->
  OldPosition;
colision(OldPosition, NewPosition, [_ | NPCs]) ->
  colision(OldPosition, NewPosition, NPCs).

% Tests

move_test() ->
  NPCs = [{2, 2, $T}],
  {{1, 0}, NPCs} = move({{1, 1}, NPCs}, ?north),
  {{1, 2}, NPCs} = move({{1, 1}, NPCs}, ?south),
  {{0, 1}, NPCs} = move({{1, 1}, NPCs}, ?west),
  {{2, 1}, NPCs} = move({{1, 1}, NPCs}, ?east).

move_x_test() ->
  1 = move_x(2, ?west),
  0 = move_x(1, ?west),
  0 = move_x(0, ?west),

  78 = move_x(77, ?east),
  79 = move_x(78, ?east),
  79 = move_x(79, ?east).

move_y_test() ->
  1 = move_y(2, ?north),
  0 = move_y(1, ?north),
  0 = move_y(0, ?north),

  22 = move_y(21, ?south),
  23 = move_y(22, ?south),
  23 = move_y(23, ?south).

