-module(board).
-export([new_game/0, player/1, npcs/1, move_player/2]).
-include_lib("eunit/include/eunit.hrl").
-record(board, {player, npcs}).

% Utilities
% TODO: Add game state to board
% TODO: Hook up colision so that it triggers plot event
new_game() ->
  #board{
    player = {0, 0},
    npcs   = [{2, 0, "D"}, {2, 2, "B"}, {2, 4, "H"},
              {2, 6, "G"}, {2, 8, "M"}]}.

player(#board{player=Player}) -> Player.

npcs(#board{npcs=NPCs}) -> NPCs.

new(Player, NPCs) ->
  #board{player = Player, npcs = NPCs}.

move_player(#board{player=OldPosition = {X, Y}, npcs=NPCs}, Direction) ->
  NewPosition = {move_x(X, Direction), move_y(Y, Direction)},
  colision(OldPosition, NewPosition, NPCs).

move_y(Y, north) when Y > 0  -> Y - 1;
move_y(Y, south) when Y < 23 -> Y + 1;
move_y(Y, _) -> Y.

move_x(X, west) when X > 0  -> X - 1;
move_x(X, east) when X < 79 -> X + 1;
move_x(X, _) -> X.

colision(OldPosition, NewPosition, NPCs) ->
  colision(OldPosition, NewPosition, NPCs, NPCs).
colision(_OldPosition, NewPosition, NPCs, []) ->
  #board{player = NewPosition, npcs = NPCs};
colision(OldPosition, {X, Y}, NPCs, [{X, Y, _} | _NPCs]) ->
  #board{player = OldPosition, npcs = NPCs};
colision(OldPosition, NewPosition, NPCs, [_Head | Tail]) ->
  colision(OldPosition, NewPosition, NPCs, Tail).

% Tests

move_player_test() ->
  NPCs = [{2, 2, $T}],
  #board{player = {1, 0}, npcs = NPCs} = move_player(new({1, 1}, NPCs), north),
  #board{player = {1, 2}, npcs = NPCs} = move_player(new({1, 1}, NPCs), south),
  #board{player = {0, 1}, npcs = NPCs} = move_player(new({1, 1}, NPCs), west),
  #board{player = {2, 1}, npcs = NPCs} = move_player(new({1, 1}, NPCs), east).

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

