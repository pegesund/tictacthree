:- module(game, [move/7, generate_reserves/2, set_game/1, get_game/1, player_colour/2, empty_board/2, players/1,
                 clean_score/0, add_score/2, get_score/2
                ]).

:- dynamic(game).
:- dynamic(score).

players([red, blue, green]).

player_colour(I, Colour) :- J is (I - 1) mod 3, players(Players), nth0(J, Players, Colour).

empty_board(Size, Board) :-
  findall(0, between(1, Size, _), Row),
  findall(Row,  between(1, Size, _), Board).

clean_score :-
  retractall(score(_)),
  empty_assoc(Score),
  assert(score(Score)).

add_score(Player, PlayerScore) :-
  score(Score), !,
  (   get_assoc(Player, Score, OldPlayerScore) ; OldPlayerScore = 0 ), !,
  NewPlayerScore is OldPlayerScore + PlayerScore,
  put_assoc(Player, Score, NewPlayerScore, NewScore),
  retractall(score(_)),
  assert(score(NewScore)).

get_score(Player, PlayerScore) :- score(Score), !, ( get_assoc(Player, Score, PlayerScore) ; PlayerScore = 0), !.

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

set_game(Game) :-
  retractall(game(_)),
  assert(game(Game)).

get_game(Game) :-
  game(Game).

new_game() :-
  retractall(game(_)).

updateZip(Board, _Player, _X, _Y, 0, _DX, _DY, Board) :- writeln(Board).

updateZip(Board, Player, X, Y, Num, DX, DY, EndBoard) :-
  NX is X + DX,
  NY is Y + DY,
  update_board(Board, NX, NY, Player, NewBoard),
  NNum is Num - 1,
  updateZip(NewBoard, Player, NX, NY, NNum, DX, DY, EndBoard), !.




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
        move((board,0,0), Board, Reserves, 2, 2, 2, _NewGame).

test(update_zip) :-
        Board = [[1,2,2,2,2,1],[0,0,0,0,0,0],[0,0,0,0,0,0],
                 [0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0]],
        updateZip(Board, 1, 0, 0, 4, 1, 0, NBoard),
        nth0(0, NBoard , [1,1,1,1,1,1]).

test(reverse) :-
        writeln("Testing"),
        reverse([a,b], [b,a]).

:- end_tests(game).

