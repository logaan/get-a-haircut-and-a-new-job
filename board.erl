-module(board).
-export([new_game/0, player/1, npcs/1, move_player/2]).
-include_lib("eunit/include/eunit.hrl").
-record(board, {state, player, npcs}).

% Utilities
% TODO: Hook up colision so that it triggers plot event
new_game() ->
  #board{ state  = start,
          player = {0, 0},
          npcs   = [{2, 0, "D"}, {2, 2, "B"}, {2, 4, "H"},
                    {2, 6, "G"}, {2, 8, "M"}]}.

player(#board{player=Player}) -> Player.

npcs(#board{npcs=NPCs}) -> NPCs.

move_player(Board=#board{player={X, Y}}, Direction) ->
  NewPosition = {move_x(X, Direction),
                 move_y(Y, Direction)},
  colision(NewPosition, Board).

move_y(Y, north) when Y > 0  -> Y - 1;
move_y(Y, south) when Y < 23 -> Y + 1;
move_y(Y, _) -> Y.

move_x(X, west) when X > 0  -> X - 1;
move_x(X, east) when X < 79 -> X + 1;
move_x(X, _) -> X.

colision(NewPosition, Board=#board{npcs=NPCs}) ->
  colision(NewPosition, NPCs, Board).

colision(NewPosition, _NPCS=[], Board) ->
  Board#board{player = NewPosition};
colision(_NewPosition={X, Y}, _NPCs=[{X, Y, _} | _], Board) ->
  Board;
colision(NewPosition, _NPCs=[_ | Tail], Board) ->
  colision(NewPosition, Tail, Board).

% Tests

move_player_test() ->
  Board0  = new_game(),
  Board1 = #board{player = {0, 1}} = move_player(Board0, south),
  Board2 = #board{player = {1, 1}} = move_player(Board1, east),
  Board3 = #board{player = {1, 0}} = move_player(Board2, north),
           #board{player = {0, 0}} = move_player(Board3, west).

move_x_test() ->
  1 = move_x(2, west),
  0 = move_x(1, west),
  0 = move_x(0, west),

  78 = move_x(77, east),
  79 = move_x(78, east),
  79 = move_x(79, east).

move_y_test() ->
  1 = move_y(2, north),
  0 = move_y(1, north),
  0 = move_y(0, north),

  22 = move_y(21, south),
  23 = move_y(22, south),
  23 = move_y(23, south).

