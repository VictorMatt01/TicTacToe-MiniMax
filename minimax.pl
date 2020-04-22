% this code is written with the hulp of the following prolog program:
% https://github.com/jaunerc/minimax-prolog/blob/master/minimax.pl 

:- module(minimax, [minimax/2]).

% All win positions for tic-tac-toe
win_pos(P, [P, P, P, _, _, _, _, _, _]).
win_pos(P, [_, _, _, P, P, P, _, _, _]).
win_pos(P, [_, _, _, _, _, _, P, P, P]).
win_pos(P, [P, _, _, P, _, _, P, _, _]).
win_pos(P, [_, P, _, _, P, _, _, P, _]).
win_pos(P, [_, _, P, _, _, P, _, _, P]).
win_pos(P, [P, _, _, _, P, _, _, _, P]).
win_pos(P, [_, _, P, _, P, _, P, _, _]).


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

% possible_move(+PlayerColor, +Board, -PossibleMove)
% this will replace the first n with PlayerCoclor, so this represents
% a possible move
possible_move(P,[n|Rest],[P|Rest]).
possible_move(P,[X|Rest],[X|Rest2]) :- possible_move(P, Rest, Rest2).

% best_mov(+MinMax, +AllMoves, -BestMove, -BestValue)
% Chooses the next move.
best_move(max, [], [], -2).
best_move(min, [], [], +2).
best_move(MinMax, [Move | RestMoves], BestMove, BestValue) :- 
    eval_board(Move,Value),
    best_move(MinMax, RestMoves, CurrentBestM, CurrentBestV),
    compare_moves(MinMax, Move, Value, CurrentBestM, CurrentBestV, BestMove, BestValue).
best_move(MinMax, [Move | RestMoves], BestMove, BestValue) :- 
    best_move(MinMax, RestMoves, CurrentBestM, CurrentBestV),
    change_max_min(MinMax, Other),
    minimax_step(Other, Move, _, BottomBestV),
    compare_moves(MinMax, Move, BottomBestV, CurrentBestM, CurrentBestV, BestMove, BestValue).

% eval_board(+Board, -Value)
% this will evaluate the score of the playboard
eval_board([],Value) :- Value is 0.
eval_board(Board, Value) :- win_pos(x, Board), Value is 1,!.
eval_board(Board, Value) :- win_pos(o, Board), Value is -1, !.
eval_board(Board, Value) :- full_board(Board), Value is 0.

full_board(Board) :- \+(member(n, Board)).

% compare_moves(+MinMax, +MoveA, +ValueA, +MoveB, +ValueB, -BetterMove, -Bettervalue)
% this will choose the move with the higher value
compare_moves(max, MoveA, ValueA, _, ValueB, MoveA, ValueA) :-
    ValueA >= ValueB.
compare_moves(max, _, ValueA, MoveB, ValueB, MoveB, ValueB) :-
    ValueA < ValueB.
compare_moves(min, MoveA, ValueA, _, ValueB, MoveA, ValueA) :-
	ValueA =< ValueB.
compare_moves(min, _, ValueA, MoveB, ValueB, MoveB, ValueB) :-
	ValueA > ValueB.

% change_max_min(+MinOrMax, TheOther)
% Changes the MinMax atom.
change_max_min(max, min).
change_max_min(min, max).