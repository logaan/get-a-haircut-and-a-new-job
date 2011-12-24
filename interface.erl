-module(interface).
-compile(export_all).
-include_lib("../cecho/include/cecho.hrl").
-include_lib("eunit/include/eunit.hrl").

start() ->
  setup(),
  start_game().

start_game() ->
  loop({0, 0}).

loop(OldPosition = {X, Y}) ->
  case key_bindings(cecho:getch()) of
    move_up    ->
      loop(move_player(OldPosition, {X, Y-1}));
    move_down  ->
      loop(move_player(OldPosition, {X, Y+1}));
    move_left  ->
      loop(move_player(OldPosition, {X-1, Y}));
    move_right ->
      loop(move_player(OldPosition, {X+1, Y}));
    quit       -> teardown();
    unbound    -> loop(OldPosition)
  end.

key_bindings($k)  -> move_up;
key_bindings($j)  -> move_down;
key_bindings($h)  -> move_left;
key_bindings($l)  -> move_right;
key_bindings($) -> quit;
key_bindings(_)   -> unbound.

move_player(OldPosition, NewPosition) ->
  print_string(OldPosition, " "),
  print_string(NewPosition, "@"),
  cecho:refresh(),
  NewPosition.

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

