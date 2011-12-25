-module(interface).
-compile(export_all).
-include_lib("../cecho/include/cecho.hrl").
-include_lib("eunit/include/eunit.hrl").

start() ->
  setup_curses(),
  game_loop({{0, 0}, [{2, 0, "D"},
                      {2, 2, "B"},
                      {2, 4, "H"},
                      {2, 6, "G"},
                      {2, 8, "M"}]}).

% TODO: Turn world into a record
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

move_y(Y, $k) when Y > 0  -> Y - 1;
move_y(Y, $j) when Y < 23 -> Y + 1;
move_y(Y, _) -> Y.

move_x(X, $h) when X > 0  -> X - 1;
move_x(X, $l) when X < 79 -> X + 1;
move_x(X, _) -> X.

colision(_, []) -> false;
colision({X, Y}, [{X, Y, _} | _]) -> true;
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

