:- use_module(library(pce)).

:- pce_global(@make_piece_gesture, make_move_piece_gesture).
:- use_module(game).
:- free(@score).
:- free(@red_score).
:- free(@green_score).
:- free(@blue_score).
:- free(@turn).

dynamic(sizes).


make_move_piece_gesture(G) :-
    new(G, move_gesture(left)),
    send(G, send_method,
         send_method(verify, vector(event),
                     message(@prolog, verify, @receiver, @arg1))),
    send(G, send_method,
         send_method(terminate, vector(event),
                     message(@prolog, terminate, @receiver, @arg1))),
    send(G, send_method,
         send_method(drag, vector(event),
                     message(@prolog, drag, @receiver, @arg1))).

drag(_Gesture, Event) :-
     get(Event, receiver, R),
     get(R, device, Dev),
     get(Event, x, Dev, X),
     get(Event, y, Dev, Y),
     b_getval(pos, (X2, Y2)),
     X3 is X - X2,
     Y3 is Y - Y2,
     send(R, x, X3),
     send(R, y, Y3).

verify(_Gesture, Event) :-
    sizes(BorderSize, CellSize, PenSize, _CircleSize, BoardSize),
    get(Event, receiver, R),
    get(R, device, Dev),
    get(Event, x, Dev, DX),
    get(Event, y, Dev, DY),
    get(Event, x, X),
    get(Event, y, Y),
    nb_linkval(pos, (X,Y)),
    XMouse is DX - X,
    YMouse is DY - Y,
    CellX is round((XMouse - BorderSize) / ((PenSize + CellSize))),
    CellY is round((YMouse - BorderSize) / ((PenSize + CellSize))),
    BSD is BoardSize - 1,
    (   (CellX > BSD ; CellY > BSD ; CellY < 0 ; CellX < 0 )
        -> DragReserve = true ; DragReserve = false
    ),
    writeln(("Dragreserve: ", DragReserve)),
    writeln((CellX, CellY)),
    send(R, expose),
    send(R, fill_pattern, gray),
    true.

terminate(_Gesture, Event) :-
    get(Event, receiver, R),
    b_getval(pos, (X2, Y2)),
    sizes(BorderSize, CellSize, PenSize, CircleSize, BoardSize),
    get(R, device, Dev),
    get(Event, x, Dev, X),
    get(Event, y, Dev, Y),
    XMouse is X - X2,
    YMouse is Y - Y2,
    Adjust is round((CellSize - CircleSize) / 2),
    CellX is round((XMouse - BorderSize) / ((PenSize + CellSize))),
    CellY is round((YMouse - BorderSize) / ((PenSize + CellSize))),
    CellX >= 0, CellX < BoardSize,
    CellY >= 0, CellY < BoardSize, !,
    XNewPos is Adjust + BorderSize + ((CellSize + PenSize) * CellX),
    YNewPos is Adjust + BorderSize + ((CellSize + PenSize) * CellY),
    get(R, colour, colour(Colour)),
    send(R, fill_pattern, colour(Colour)),
    send(R, x, XNewPos),
    send(R, y, YNewPos).


add_balls(StartX, StartY, Num, Colour, CircleSize) :-
    N is Num - 1,
    (   between(0, N, I),
        X is StartX + round(I * CircleSize * 1.2),
        addCircle(CircleSize, X, StartY, Colour, _Circle),
        false ; true
    ).


run(BoardSize, NumBalls) :-
    BorderSize = 120,
    CellSize = 80,
    PenSize = 3,
    CircleSize is round(CellSize * 0.75),
    AdminSize is round(CellSize * 3),
    XSize is (CellSize * BoardSize) + (PenSize * BoardSize) + (2 * BorderSize),
    YSize is (CellSize * BoardSize) + (PenSize * BoardSize) + (2 * BorderSize) + AdminSize,
    StartYBalls is YSize - AdminSize - round(BorderSize/2),
    new(@pict, window('Foxface', size(XSize, YSize))),
    (   between(0, BoardSize, I),
        XStart is (I * (CellSize + PenSize)) + BorderSize,
        YStart is BorderSize,
        YStop is BorderSize + (BoardSize * (CellSize +  PenSize)),
        send(@pict, display, new(Line, line(XStart, YStart, XStart, YStop))),
        send(Line, colour, blue), send(Line, pen, 3),
        fail ; true
    ),
    (   between(0, BoardSize, I),
        XStart is BorderSize,
        YStart is (I * (CellSize + PenSize)) + BorderSize,
        XStop is BorderSize + (BoardSize * (CellSize +  PenSize)),
        send(@pict, display, new(Line, line(XStart, YStart, XStop, YStart))),
        send(Line, colour, blue), send(Line, pen, 3),
        fail ; true
    ),
    CircleSize is round(CellSize * 0.75),
    add_balls(BorderSize, StartYBalls, NumBalls, red, CircleSize),
    StartYBallsBlue is StartYBalls + CellSize,
    add_balls(BorderSize, StartYBallsBlue, NumBalls, blue, CircleSize),
    StartYBallsGreen is StartYBallsBlue + CellSize,
    add_balls(BorderSize, StartYBallsGreen, NumBalls, green, CircleSize),
    assert(sizes(BorderSize, CellSize, PenSize, CircleSize, BoardSize)),
    generate_reserves(NumBalls, Reserves),
    Turn = 0,
    empty_board(BoardSize, Board),
    set_game((Board, Reserves, Turn)),
    addScore(),
    send(@pict, open).

addCircle(CircleSize, X, Y, Colour, Circle) :-
    send(@pict, display,
          new(Circle, circle(CircleSize)), point(X,Y)),
    send(Circle, colour(Colour)),
    send(Circle, fill_pattern, colour(Colour)),
    send(Circle, recogniser, new(@make_piece_gesture)).

addScore() :-
    send(@pict, display, new(@score, text("Score:"))), send(@score, x, 15), send(@score, y, 50),
    send(@score, font, font(helvetica,roman,30)),
    send(@pict, display, new(@red_score, text("0"))), send(@red_score, x, 150), send(@red_score, y, 50),
    send(@red_score, font, font(helvetica,roman,30)), send(@red_score, colour, red),
    send(@pict, display, new(@green_score, text("0"))), send(@green_score, x, 250), send(@green_score, y, 50),
    send(@green_score, font, font(helvetica,roman,30)), send(@green_score, colour, green),
    send(@pict, display, new(@blue_score, text("0"))), send(@blue_score, x, 350), send(@blue_score, y, 50),
    send(@blue_score, font, font(helvetica,roman,30)), send(@blue_score, colour, blue),
    send(@pict, display, new(@turn, circle(30)), point(400, 50)),
    send(@turn, fill_pattern, colour(red)).

