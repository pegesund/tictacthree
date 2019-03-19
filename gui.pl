:- use_module(library(pce)).

:- pce_global(@make_piece_gesture, make_move_piece_gesture).
:- use_module(game).
:- use_module(eval).

dynamic(sizes).
dynamic(balls).
dynamic(reserved).
dynamic(move_time).
dynamic(round).
dynamic(progress).


free_globals :-
 free(@score),
 free(@red_score),
 free(@green_score),
 free(@blue_score),
 free(@turn),
 free(@time_move), free(@timer_move),
 free(@pict),
 free(@rounds),
 free(@rounds_back),
 catch(mutex_destroy(update), _E, true),
 mutex_create(update).



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

allowMoveFrom(Circle) :-
    get_game((_Board, _Reserves, Turn)), !,
    player_colour(Turn, Col),
    get(Circle, colour, colour(Col)).


verify(_Gesture, Event) :-
    sizes(BorderSize, CellSize, PenSize, _CircleSize, BoardSize),
    get(Event, receiver, R),
    get(R, position, point(XScreen, YScreen)),
    get(R, device, Dev),
    get(Event, x, Dev, DX),
    get(Event, y, Dev, DY),
    get(Event, x, X),
    get(Event, y, Y),
    allowMoveFrom(R),
    nb_linkval(pos, (X,Y)),
    nb_linkval(pos_screen, (XScreen, YScreen)),
    get_game((_Board, _Reserves, Turn)), !,
    nb_linkval(move_turn, Turn),
    XMouse is DX - X,
    YMouse is DY - Y,
    CellX is round((XMouse - BorderSize) / ((PenSize + CellSize))),
    CellY is round((YMouse - BorderSize) / ((PenSize + CellSize))),
    BSD is BoardSize - 1,
    (   (CellX > BSD ; CellY > BSD ; CellY < 0 ; CellX < 0 )
        -> DragReserve = reserves ; DragReserve = board
    ),
    % writeln(("Dragreserve: ", DragReserve)),
    % writeln((CellX, CellY)),
    nb_linkval(from_move, (DragReserve, CellX, CellY)),
    send(R, expose),
    send(R, fill_pattern, gray),
    true.

newMove(X,Y) :-
    get_game((Board, Reserves, Turn)), !,
    b_getval(move_turn, Turn),
    b_getval(from_move, (DragReserve, FromXBoard, FromYBoard)),
    move((DragReserve, FromXBoard, FromYBoard), Board, Reserves, Turn, X, Y, (NewBoard, NewReserves)),
    % writeln(((DragReserve, FromXBoard, FromYBoard), Board, Reserves, Turn, X, Y, (NewBoard, NewReserves))),
    NewTurn is (Turn mod 3) + 1,
    player_colour(NewTurn, NewCol),
    set_game((NewBoard, NewReserves, NewTurn)),
    send(@turn, fill_pattern, colour(NewCol)),
    move_time(MoveTime), !,
    atom_string(MoveTime, MoveTimeStr),
    send(@time_move, string, string(MoveTimeStr)).


terminate(_Gesture, Event) :-
    with_mutex(update, (
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
    (   newMove(CellX, CellY), XEndPos = XNewPos, YEndPos = YNewPos ;
         b_getval(pos_screen,(XEndPos, YEndPos))
    ),
    get(R, colour, colour(Colour)),
    send(R, fill_pattern, colour(Colour)),
    send(R, x, XEndPos),
    send(R, y, YEndPos)
    )).


add_balls(StartX, StartY, Num, Colour, CircleSize) :-
    N is Num - 1,
    (   between(0, N, I),
        X is StartX + round(I * CircleSize * 1.2),
        addCircle(CircleSize, X, StartY, Colour, _Circle),
        false ; true
    ).


run(BoardSize, NumBalls) :-
    free_globals,
    clean_score,
    retractall(ball(_)),
    retractall(reserved(_,_)),
    retractall(sizes(_, _, _, _, _)),
    retractall(move_time(_)),
    retractall(round(_,_)),
    assert(move_time(8)),
    assert(round(5,60)),
    retractall(progress(_)),
    assert(progress(0)),
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
    ), !,
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
    Turn = 1,
    empty_board(BoardSize, Board),
    set_game((Board, Reserves, Turn)),
    addScore(),
    assert(reserved(red, StartYBalls)),
    assert(reserved(green, StartYBallsGreen)),
    assert(reserved(blue, StartYBallsBlue)),
    writeln("CREATED"),
    send(@pict, open).

addCircle(CircleSize, X, Y, Colour, Circle) :-
    send(@pict, display,
          new(Circle, circle(CircleSize)), point(X,Y)),
    send(Circle, colour(Colour)),
    send(Circle, fill_pattern, colour(Colour)),
    send(Circle, recogniser, new(@make_piece_gesture)),
    assert(ball(Circle)).

update_move_timer :-
    with_mutex(update, (
    get(@time_move, string, string(OldTime)),
    atom_number(OldTime, I),
    NewI is I - 1,
    (   NewI == 0 ->
         move_time(MoveTime), !, NewTime2 = MoveTime,
         get_game((Board, Reserves, Turn)), !, NewTurn is (Turn mod 3) + 1,
         player_colour(NewTurn, NewCol),
         set_game((Board, Reserves, NewTurn)),
         send(@turn, fill_pattern, colour(NewCol)) ;
         NewTime2 = NewI
    ),
    number_string(NewTime2, NewTimeStr),
    send(@time_move, string, string(NewTimeStr)),
    progress(P),
    NewProgress is P + 1,
    retractall(progress(_)),
    assert(progress(NewProgress)),
    adjustPercentage,
    adjustScore)).

addScore() :-
    send(@pict, display, new(@score, text("Score:"))), send(@score, x, 15), send(@score, y, 50),
    send(@score, font, font(helvetica,roman,30)),
    send(@pict, display, new(@red_score, text("0"))), send(@red_score, x, 150), send(@red_score, y, 50),
    send(@red_score, font, font(helvetica,roman,30)), send(@red_score, colour, red),
    send(@pict, display, new(@green_score, text("0"))), send(@green_score, x, 250), send(@green_score, y, 50),
    send(@green_score, font, font(helvetica,roman,30)), send(@green_score, colour, green),
    send(@pict, display, new(@blue_score, text("0"))), send(@blue_score, x, 350), send(@blue_score, y, 50),
    send(@blue_score, font, font(helvetica,roman,30)), send(@blue_score, colour, blue),
    send(@pict, display, new(@turn, circle(30)), point(500, 50)),
    send(@turn, fill_pattern, colour(red)),
    move_time(MoveTime), !,
    number_string(MoveTime, MoveTimeStr),
    send(@pict, display, new(@time_move, text(MoveTimeStr))), send(@time_move, position, point(550, 50)), send(@time_move, font, font(helvetica,roman,30)),
    new(@timer_move, timer(1, message(@prolog, update_move_timer))), send(@timer_move, start),
    createRoundCounters.


ballAtCoordinate(X,Y,C) :-
    sizes(BorderSize, CellSize, PenSize, _CircleSize, _BoardSize), !,
    CX is (X * (PenSize + CellSize)) + BorderSize,
    CY is (Y * (PenSize + CellSize)) + BorderSize,
    ball(C),
    get(C, position, point(PX,PY)),
    PX >= CX, PX < CX + CellSize ,
    PY >= CY, PY < CY + CellSize, !.

existBallWithin(SX, SY, CircleBorder) :-
    ball(C),
    get(C, position, point(BX,BY)),
    SXEnd is SX + CircleBorder - 1, SYEnd is SY + CircleBorder,
    between(SX, SXEnd, BX),
    between(SY, SYEnd, BY),
    !.

findFreeReserve(Colour, Num) :-
    sizes(BorderSize, _CellSize, _PenSize, CircleSize, BoardSize), !,
    reserved(Colour, SY), !,
    CircleBorder is round(CircleSize * 1.2),
    between(0, BoardSize, I),
    SX is BorderSize + round(CircleSize * 1.2 * I),
    not(existBallWithin(SX, SY, CircleBorder)),
    Num = I, !,
    Num < BoardSize.

createRoundCounters :-
    new(@rounds, device),
    round(Rounds, _RSize), !,
    RoundsDec is Rounds - 1,
    BoxWidth = 30,
    BoxHeight = 20,
    new(@rounds_back, box(0, BoxHeight)),
    send(@rounds, display, @rounds_back, point(0,0)),
    send(@rounds_back, pen, 0),
    send(@rounds_back, fill_pattern, colour(orange)),
    (   between(0,RoundsDec, I),
        new(Box, box(BoxWidth, BoxHeight)),
        send(Box, fill_pattern, @nil),
        send(@rounds, display, Box),
        X is I * BoxWidth,
        send(Box, position, point(X,0)),
        fail ; true
    ),
    send(@pict, display, @rounds),
    send(@rounds, position, point(630,60)).

adjustPercentage :-
    BoxWidth = 30,
    progress(Prog), !,
    round(Rounds, RSize),
    TotalSecs is Rounds * RSize,
    WTotal is BoxWidth * Rounds,
    ProgPercentage is Prog / TotalSecs,
    NewWidth is round(ProgPercentage  * WTotal),
    send(@rounds_back, width, NewWidth).


updateScore(red, Score) :-
    send(@red_score, string, Score), !.
updateScore(blue, Score) :-
    send(@blue_score, string, Score), !.
updateScore(green, Score) :-
    send(@green_score, string, Score), !.


adjustScore :-
     round(_Rounds, RSize),
     progress(P),
     D is P mod RSize,
     D == 0,
     players(Players),
     length(Players, PL),
     get_game((Board, _Reserves, _Turn)), !,
     (   between(1, PL, I),
         getScore(Board, I, Score),
         add_score(I, Score),
         PI is I - 1,
         nth0(PI, Players, Colour),
         get_score(I, NewScore),
         updateScore(Colour, NewScore),
         fail ; true
     ), fail.

adjustScore :-
     round(Rounds, RSize),
     progress(P),
     Total is Rounds * RSize,
     P >= Total,
     send(@timer_move, stop), !.

adjustScore :- !.



checkZipNonDeterm(Player, X,Y, NumZip, DirX, DirY) :-
     get_game((Board, _Reserves, _Turn)), !,
     member(DirX, [-1,0,1]), member(DirY, [-1,0,1]),
     checkZipStart(Board, Player, X, Y, DirX, DirY, NumZip),
     (   between(1, NumZip, I),
         TurnX is X + (I * DirX),
         TurnY is Y + (I * DirY),
         nth0(TurnY, Board, Row), nth0(TurnX, Row, Cell),
         player_colour(Cell, Col),
         writeln((TurnX, TurnY)),
         findFreeReserve(Col, _NumFreeReserver),
         ballAtCoordinate(TurnX,TurnY,Ball),
         send(Ball, fill_pattern, colour(purple)),
         fail ; true
     ).


