:- module(eval, [getScore/3, checkZipStart/7]).

evalPoint(Matrix, X, Y, XA, YA, P, Score) :-
    nth0(Y, Matrix, Row), nth0(X, Row, C),
    C == P,
    X1 is X + XA, X2 is X - XA, Y1 is Y + YA, Y2 is Y - YA,
    X3 is X + (2 * XA), X4 is X - (2 * XA), Y3 is Y + (2 * YA), Y4 is Y - (2 * YA),
    nth0(Y1, Matrix, R1), nth0(X1, R1, C1), C1 == P,
    nth0(Y2, Matrix, R2), nth0(X2, R2, C2), C2 == P,
    (    nth0(Y3, Matrix, R3), nth0(X3, R3, C3), C3 == P ;
         nth0(Y4, Matrix, R4), nth0(X4, R4, C4), C4 == P
    ),
    Score = 1, !.

% wow - this is a litteraly a corner case :)
evalPoint(Matrix, X, Y, XA, YA, P, Score) :-
    nth0(Y, Matrix, Row), nth0(X, Row, C),
    C == P,
    (   X1 is X + XA, X2 is X + (2 * XA), Y1 is Y + YA, Y2 is Y + (2 * YA),
        X3 is X + (3 * XA), Y3 is Y + (3 * YA) ;
        X1 is X - XA, X2 is X - (2 * XA), Y1 is Y - YA, Y2 is Y - (2 * YA),
        X3 is X - (3 * XA), Y3 is Y - (3 * YA)
    ),
    nth0(Y1, Matrix, R1), nth0(X1, R1, C1), C1 == P,
    nth0(Y2, Matrix, R2), nth0(X2, R2, C2), C2 == P,
    nth0(Y3, Matrix, R3), nth0(X3, R3, C3), C3 == P,
    Score = 1, !.
evalPoint(_, _, _, _, _, _, 0).

evalMatrix(Matrix, X, Y, _, Acc, Score) :- length(Matrix, MLen), X >= MLen, Y >= MLen - 1, Score = Acc.
evalMatrix(Matrix, X, Y, P, Acc, Score) :- length(Matrix, MLen), X >= MLen, NewY is Y + 1, evalMatrix(Matrix, 0, NewY, P, Acc, Score).
evalMatrix(Matrix, X, Y, P, Acc, Score) :-
    evalPoint(Matrix, X, Y, 0, 1, P, S1),
    evalPoint(Matrix, X, Y, 1, 0, P, S2),
    evalPoint(Matrix, X, Y, 1, 1, P, S3),
    evalPoint(Matrix, X, Y, 1, -1, P, S4),
    NewAcc is Acc + S1 + S2 + S3 + S4,
    NewX is X + 1,
    evalMatrix(Matrix, NewX, Y, P, NewAcc, Score), !.

getScore(Matrix, Person, Score) :- evalMatrix(Matrix, 0, 0, Person, 0, Score).

checkZipStart(_Board, _Player, _X, _Y, 0, 0, _NumZip) :- !, fail.

checkZipStart(Board, Player, X, Y, DirX, DirY, NumZip) :-
    NewY is Y + DirY, NewX is X + DirX,
    nth0(NewY, Board, Row), nth0(NewX, Row, OtherPlayer),
    OtherPlayer \== Player,
    checkZip(Board, Player, OtherPlayer, NewX, NewY, DirX, DirY, 0, NumZip).

checkZip(Board, Player, _OtherPlayer, X, Y, _DirX, _DirY, NumZip, NumZip) :-
    nth0(Y, Board, Row), nth0(X, Row, OtherPlayerNew),
    OtherPlayerNew == Player,
    NumZip > 2, !.

checkZip(Board, _Player, OtherPlayer, X, Y, _DirX, _DirY, _NumZip, _NumZipNew) :-
     nth0(Y, Board, Row), nth0(X, Row, OtherPlayerNew),
     OtherPlayerNew \== OtherPlayer,
     !, fail.

checkZip(Board, Player, OtherPlayer, X, Y, DirX, DirY, NumZip, NumZipNew) :-
    nth0(Y, Board, Row), nth0(X, Row, Cell),
    Cell > 0,
    NewY is Y + DirY, NewX is X + DirX,
    NumZipNew2 is NumZip + 1,
    checkZip(Board, Player, OtherPlayer, NewX, NewY, DirX, DirY, NumZipNew2, NumZipNew).



:- begin_tests(eval).
:- use_module(eval).

test(one_row) :-
    getScore([[1,1,1,1], [0,0,0,0], [0,0,0,0], [0,0,0,0]],1,Score),
    Score == 4.

test(no_score) :-
    getScore([[1,1,1,0], [0,0,0,0], [0,0,0,0], [0,0,0,0]],1,Score),
    Score == 0.

test(one_col) :-
    getScore([[1,0,0,0], [1,0,0,0], [1,0,0,0], [1,0,0,0]],1,Score),
    Score == 4.

test(mid_col) :-
    getScore([[0,1,0,0], [0,1,0,0], [0,1,0,0], [0,1,0,0]],1,Score),
    Score == 4.

test(zip) :-
    getScore([[1,0,0,0], [0,1,0,0], [0,0,1,0], [0,0,0,1]],1,Score),
    Score == 4.

test(other_zip) :-
    getScore([[0,0,0,1], [0,0,1,0], [0,1,0,0], [1,0,0,0]],1,Score),
    Score == 4.

test(zip_and_row) :-
    getScore([[1,1,1,1], [0,0,1,0], [0,1,0,0], [1,0,0,0]],1,Score),
    Score == 8.

test(all) :-
    getScore([[1,1,1,1], [1,1,1,1], [1,1,1,1], [1,1,1,1]],1,Score),
    Score == 40.

test(five) :-
     getScore([[1,1,1,1,0], [0,0,0,0,0], [0,1,1,1,1], [0,0,0,0,0], [1,1,1,0,0]], 1, Score),
     Score == 8.

test(zipzap_col) :-
    checkZipStart([[1,0,0,0,0,0], [2,0,0,0,0,0], [2,0,0,0,0,0],
                   [2,0,0,0,0,0], [2,0,0,0,0,0], [1,0,0,0,0,0]], 1,0,0,0,1,NumZip),
    NumZip = 4.

test(zipzap_col, fail) :-
    checkZipStart([[1,0,0,0,0,0], [2,0,0,0,0,0], [2,0,0,0,0,0],
                   [2,0,0,0,0,0], [2,0,0,0,0,0], [2,0,0,0,0,0]], 1,0,0,0,1,_).

test(zipzap_row) :-
    checkZipStart([[1,2,2,2,2,1], [0,0,0,0,0,0], [0,0,0,0,0,0],
                   [0,0,0,0,0,0], [0,0,0,0,0,0], [0,0,0,0,0,0]], 1,0,0,1,0,NumZip),
    NumZip = 4.


:- end_tests(eval).










