-module(interface).
-export([start/0]).
-include_lib("../cecho/include/cecho.hrl").

start() ->
  setup_curses(),
  game_loop(board:new_game()).

game_loop(Board) ->
  redraw_world(Board),
  case key_bindings(cecho:getch()) of
    quit -> teardown_curses();
    MoveDirection ->
      game_loop(board:move_player(Board, MoveDirection))
  end.

key_bindings($) -> quit;
key_bindings($k)  -> north;
key_bindings($j)  -> south;
key_bindings($h)  -> west;
key_bindings($l)  -> east.

setup_curses() ->
  application:start(cecho),
  cecho:cbreak(),
  cecho:noecho(),
  cecho:curs_set(?ceCURS_INVISIBLE).

teardown_curses() ->
  application:stop(cecho),
  erlang:halt().

redraw_world(Board) ->
  cecho:erase(),
  draw_npcs(board:npcs(Board)),
  draw_player(board:player(Board)),
  cecho:refresh().

draw_player({X, Y}) ->
  cecho:mvaddstr(Y, X, "@").

draw_npcs([]) -> ok;
draw_npcs([{X, Y, Char} | NPCs]) ->
  cecho:mvaddstr(Y, X, Char),
  draw_npcs(NPCs).

