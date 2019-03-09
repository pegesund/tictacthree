:- use_module(library(pce)).

:- pce_global(@make_piece_gesture, make_move_piece_gesture).

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
     get(Event, receiver, R),
     send(R, fill_pattern, gray),
     get(Event, x, X),
     get(Event, y, Y),
     nb_linkval(pos, (X,Y)),
     writeln(X),
     true.

terminate(_Gesture, Event) :-
    % get(Event, x, X),
    get(Event, receiver, R),
    send(R, fill_pattern, colour(red)),
    writeln(R),
    writeln("terminated").

run(BoardSize) :-
    BorderSize = 30,
    CellSize = 30,
    XSize is CellSize * BoardSize,
    YSize is CellSize * BoardSize,

    new(@pict, window('Foxface', size(400, 200))),
    addCircle(_C),
    send(@pict, open).

addCircle(Circle) :-
    send(@pict, display,
          new(Circle, circle(50)), point(25,25)),
    send(Circle, colour(red)),
    send(Circle, fill_pattern, colour(red)),
    send(Circle, recogniser, new(@make_piece_gesture)),
    send(Circle, send_method,
         send_method(verify, vector(event),
                     message(@prolog, verify, @receiver, @arg1))),
    send(Circle, send_method,
         send_method(terminate, vector(event),
                     message(@prolog, terminate, @receiver, @arg1))).

