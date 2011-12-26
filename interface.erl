-module(interface).
-export([start/0]).
-include_lib("../cecho/include/cecho.hrl").

start() ->
  setup_curses(),
  draw_world(board:new_game()).

draw_world(Board) ->
  cecho:erase(),
  case board:state(Board) of
    won  -> draw_win();
    lost -> draw_loss();
    _    -> draw_npcs(board:npcs(Board)),
            draw_player(board:player(Board))
  end,
  draw_message(board:message(Board)),
  cecho:refresh(),
  check_game_end(Board).

game_loop(Board) ->
  case key_bindings(cecho:getch()) of
    unknown -> draw_world(Board);
    quit    -> teardown_curses();
    MoveDirection -> draw_world(board:move_player(Board, MoveDirection))
  end.

key_bindings($) -> quit;
key_bindings($k)  -> north;
key_bindings($j)  -> south;
key_bindings($h)  -> west;
key_bindings($l)  -> east;
key_bindings(_)   -> unknown.

setup_curses() ->
  application:start(cecho),
  cecho:cbreak(),
  cecho:noecho(),
  cecho:curs_set(?ceCURS_INVISIBLE).

teardown_curses() ->
  application:stop(cecho),
  erlang:halt().

close_on_confirm() ->
  cecho:getch(),
  teardown_curses().

draw_player({X, Y}) ->
  cecho:mvaddstr(Y, X, "@").

draw_npcs([]) -> ok;
draw_npcs([{X, Y, Char} | NPCs]) ->
  cecho:mvaddstr(Y, X, [Char]),
  draw_npcs(NPCs).

draw_message(Message) ->
  cecho:mvaddstr(23, 0, Message).

draw_win() ->
  cecho:mvaddstr(11, 33, "You won the game").

draw_loss() ->
  cecho:mvaddstr(11, 32, "You lost the game").

check_game_end(Board)    -> check_game_end(board:state(Board), Board).
check_game_end(won,  _)  -> close_on_confirm();
check_game_end(lost, _)  -> close_on_confirm();
check_game_end(_, Board) -> game_loop(Board).

