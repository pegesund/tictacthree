
evalPoint(Matrix, X, Y, XA, YA, P, Score) :-
    nth0(Y, Matrix, Row), nth0(X, Row, C),
    C == P,
    X1 is X + XA, X2 is X - XA, Y1 is Y + YA, Y2 is Y - YA,
    nth0(Y1, Matrix, R1), nth0(X1, R1, C1), C1 == P,
    nth0(Y2, Matrix, R2), nth0(X2, R2, C2), C2 == P,
    Score = 1, !.
% wow - this is a litteraly a corner case :)
evalPoint(Matrix, X, Y, XA, YA, P, Score) :-
    nth0(Y, Matrix, Row), nth0(X, Row, C),
    C == P,
    (   X1 is X + XA, X2 is X + (2 * XA), Y1 is Y + YA, Y2 is Y + (2 * YA) ;
        X1 is X - XA, X2 is X - (2 * XA), Y1 is Y - YA, Y2 is Y - (2 * YA)
    ),
    nth0(Y1, Matrix, R1), nth0(X1, R1, C1), C1 == P,
    nth0(Y2, Matrix, R2), nth0(X2, R2, C2), C2 == P,
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

