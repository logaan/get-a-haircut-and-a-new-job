-module(interface).
-compile(export_all).
-include_lib("../cecho/include/cecho.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(up, $k).
-define(down, $j).
-define(left, $h).
-define(right, $l).

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
    $ -> teardown_curses();
    Direction -> game_loop(move(World, Direction))
  end.

move({OldPosition = {X, Y}, NPCs}, Direction) ->
  NewPosition = {move_x(X, Direction), move_y(Y, Direction)},
  case colision(NewPosition, NPCs) of
    true -> {OldPosition, NPCs};
    false -> {NewPosition, NPCs}
  end.

move_y(Y, ?up)   when Y > 0  -> Y - 1;
move_y(Y, ?down) when Y < 23 -> Y + 1;
move_y(Y, _) -> Y.

move_x(X, ?left)  when X > 0  -> X - 1;
move_x(X, ?right) when X < 79 -> X + 1;
move_x(X, _) -> X.

colision(_Player, []) -> false;
colision({X, Y}, [{X, Y, _} | _NPCs]) -> true;
colision(Player, [_ | NPCs]) -> colision(Player, NPCs).

% IOey things. yuck.

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

% Tests

move_x_test() ->
  1 = move_x(2, ?left),
  0 = move_x(1, ?left),
  0 = move_x(0, ?left),

  78 = move_x(77, ?right),
  79 = move_x(78, ?right),
  79 = move_x(79, ?right).

move_y_test() ->
  1 = move_y(2, ?up),
  0 = move_y(1, ?up),
  0 = move_y(0, ?up),

  22 = move_y(21, ?down),
  23 = move_y(22, ?down),
  23 = move_y(23, ?down).

