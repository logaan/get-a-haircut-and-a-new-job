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
    $k -> game_loop(move_up(World));
    $j -> game_loop(move_down(World));
    $h -> game_loop(move_left(World));
    $l -> game_loop(move_right(World));
    _ -> game_loop(World)
  end.

% TODO: Fix repetition
move_up({OldPosition = {X, Y}, NPCs}) when Y > 0 ->
  NewPosition = {X, Y-1},
  case colision(NewPosition, NPCs) of
    true -> {OldPosition, NPCs};
    false -> {NewPosition, NPCs}
  end;
move_up(World) -> World.

move_down({OldPosition = {X, Y}, NPCs}) when Y < 23 ->
  NewPosition = {X, Y+1},
  case colision(NewPosition, NPCs) of
    true -> {OldPosition, NPCs};
    false -> {NewPosition, NPCs}
  end;
move_left(World) -> World.

move_left({OldPosition = {X, Y}, NPCs}) when X > 0 -> 
  NewPosition = {X-1, Y},
  case colision(NewPosition, NPCs) of
    true -> {OldPosition, NPCs};
    false -> {NewPosition, NPCs}
  end;
move_left(World) -> World.

move_right({OldPosition = {X, Y}, NPCs}) when X < 79 ->
  NewPosition = {X+1, Y},
  case colision(NewPosition, NPCs) of
    true -> {OldPosition, NPCs};
    false -> {NewPosition, NPCs}
  end;
move_right(World) -> World.

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

