:- module(game, [move/7, generate_reserves/2]).

replace_element_at(0, [_|L], E, [E|L]).
replace_element_at(N, [X|L], E, [X|R]) :-
  succ(M, N),
  replace_element_at(M, L, E, R).

update_board(Matrix, X, Y, NewVal, NewMatrix) :-
        nth0(Y, Matrix, Row),
        replace_element_at(X,Row,NewVal, NewRow),
        replace_element_at(Y, Matrix, NewRow, NewMatrix), !.

use_reserve((reserves,_,_), Reserves, PlayerId, NewReserves) :-
    member([PlayerId,Num], Reserves),
    Num > 0,
    delete(Reserves, [PlayerId, Num], TempR),
    NewNum is Num - 1,
    NewReserves = [[PlayerId, NewNum]|TempR], !.

move_from((board,FromX, FromY), Board, Player, ToX, ToY, NewBoard) :-
        abs(FromX - ToX) < 2,
        abs(FromY - ToY) < 2,
        nth0(FromY, Board, Row),
        nth0(FromX, Row, Cell),
        Cell == Player,
        update_board(Board, FromX, FromY, 0, NewBoard).

move(Source, Board, Reserves, Player, ToX, ToY, NewGame) :-
        (   (BoardAfterFrom = Board, use_reserve(Source, Reserves, Player, NewReserves)), ! ;
            NewReserves = Reserves, move_from(Source, Board, Player, ToX, ToY, BoardAfterFrom), !
        ),
        nth0(ToY, Board, Row),
        nth0(ToX, Row, Cell),
        Cell == 0,
        update_board(BoardAfterFrom, ToX, ToY, Player, NewBoard),
        NewGame = (NewBoard, NewReserves).


generate_reserves(NumBalls, [[1,NumBalls],[2,NumBalls],[3,NumBalls]]).

:- begin_tests(game).
:- use_module(game).

test(update_board_move) :-
        writeln("Testing update of board"),
        generate_reserves(3, Reserves),
        Board = [[1,2,3],[0,0,0],[1,2,3]],
        move((board,0,0), Board, Reserves, 1, 1, 1, NewGame),
        NewGame == ([[0,2,3],[0,1,0],[1,2,3]],
                    [[1,3],[2,3],[3,3]]).

test(update_board_move_wrong_from_owner, fail) :-
        writeln("Testing update of board but wrong ower of from coordinates"),
        generate_reserves(3, Reserves),
        Board = [[1,2,3],[0,0,0],[1,2,3]],
        move((board,0,0), Board, Reserves, 2, 1, 1, _NewGame).

test(update_board_move_to_taken_pos, fail) :-
        writeln("Testing update of board with move to taken pos"),
        generate_reserves(3, Reserves),
        Board = [[1,2,3],[0,0,0],[1,2,3]],
        move((board,0,0), Board, Reserves, 1, 2, 2, _NewGame).

test(update_board_move_outside_board, fail) :-
        writeln("Testing update of board outside pos"),
        generate_reserves(3, Reserves),
        Board = [[1,2,3],[0,0,0],[1,2,3]],
        move((board,0,0), Board, Reserves, 1, 3, 3, _NewGame).

test(update_board_from_reserves) :-
        writeln("Testing update of board"),
        generate_reserves(3, Reserves),
        Board = [[1,2,3],[0,0,0],[1,2,3]],
        move((reserves,_,_), Board, Reserves, 1, 1, 1, NewGame),
        writeln(NewGame),
        NewGame == ([[1,2,3],[0,1,0],[1,2,3]],
                    [[1,2],[2,3],[3,3]]).

test(update_board_from_reserves_non_existing, fail) :-
        writeln("Testing update of board"),
        generate_reserves(1, Reserves),
        Board = [[1,2,3],[0,0,0],[1,2,3]],
        move((reserves,_,_), Board, Reserves, 1, 1, 1, (NewBoard, NewReserves)),
        writeln((NewBoard,NewReserves)),
        move((reserves,_,_), NewBoard, NewReserves, 1, 2, 1, _NewGame).

test(update_board_move_more_than_one_in_distance, fail) :-
        writeln("Testing update of board with to long move"),
        generate_reserves(3, Reserves),
        Board = [[1,2,3],[0,0,0],[0,0,0]],
        move((board,0,0), Board, Reserves, 1, 2, 2, _NewGame).


test(reverse) :-
        writeln("Testing"),
        reverse([a,b], [b,a]).

:- end_tests(game).

