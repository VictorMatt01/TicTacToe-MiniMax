% this code is written with the hulp of the following prolog program:
% https://github.com/jaunerc/minimax-prolog/blob/master/minimax.pl 

:- module(minimax, [minimax/2]).


% minimax(+Board, -BestMove)
% this will match the next best move based on the current board
minimax(Board, BestMove) :- 
    minimax_step(max, Board, BestMove, _).

% minimax_step(+Minimax, +Board, -BestMove, -BestValue)
% first look who is playing, then generate all possible moves
% aand then find the best move for the current player
minimax_step(MinMax, Board, BestMove, BestValue) :-
    player_color(MinMax, Color),
    all_possible_moves(Color, Board, AllMoves),
    best_move(MinMax, AllMoves, BestMove, BestValue).

% player_color(MinMax, Color)
% this will match the player "color" on the minmax atom
player_color(max, x).
player_color(min, o).

% all_possible_moves(+PlayerColor, +Board, -AllMoves)
% this will match AllMoves with all the possible moves for the current board
all_possible_moves(P,Board,AllMoves) :-
    findall(Move, possible_move(P, Board, Move), AllMoves).

possible_move(_,_,_).

best_move(_,_,_,_).