sm:-   %run this
    sm(600,4,120). %runs animation over 600 pixels, 4 seconds long, with 40 FPS

sm(Length,Duration,FPS):-
    (object( @window),!,free(@window);true), %check is window exists; if it does, delete it
    (object( @floor), !, free( @floor); true), %check is floor exists; if it does, delete it
    (object( @box), !, free( @box); true), %%check if box exists; if it does, delete it
    (object( @my_timer), !, free( @my_timer); true), %%check if box exists; if it does, del
    new( @window,picture('Window',size(900,300))), %create window
    send( @window,open), %display window
     send(@window,buffered_update,true), %make sure to use double-buffering
    new( @floor,box(800,10)), %create floor
    send( @floor,fill_pattern,colour(green)), %colour floor
    send( @window,display,@floor,point(50,70)),  %show floor
    new( @box,box(50,50)), %creates box
    send( @box,fill_pattern,colour(yellow)), %colours box yellow
    Frames is Duration*FPS * 2, %how many frames are to be drawn
    Step is Length/Frames, %what's the distance between steps
    Delay is 1/FPS, %what is the delay between steps
    send( @window,display,@box,point(50,20)), %shows Object
    send( @window,flush), %flushes window
    ani(Step,Delay,Frames,point(50,20)).

ani(Step,Delay,Frames,point(X,Y)) :-
    send( @box,x(X+Step)), %moves box to the right
    send( @window,flush), %flushes
    new(@my_timer, my_timer(Step, Delay, Frames, X)).

:- pce_begin_class(my_timer, object).
variable(mytimer,   timer,  both, "timer lançant l'animation fréquence 20 fois par seconde").
variable(step, number, both, "delta x").
variable(frames, number, both, "number of moves").
variable(posX, number, both, "xpos of the box").

% initialisation of the tmer
initialise(P, Step, Delay, Frames, PosX) :->
    send(P, slot, step, Step),
    send(P, slot, frames, Frames),
    send(P, slot, posX, PosX),
    send(P, mytimer, new(_, timer(Delay,message(P, my_message)))),
    send(P?mytimer, start).

% must be called before destruction
% avoid any problem of non-freed resources
unlink(F) :->
    send(F?mytimer, stop),
    send(F, send_super, unlink).


my_message(P) :->
    get(P, slot, frames, Frame),
    (   get(Frame, value, 0)
    ->  send(P?mytimer, stop),
        free( @box), %delete box
        free( @floor), %delete floor
        free( @window), %delete window (and exit)
        free(@my_timer)
    ;   get(P, slot, step, Step),
        get(P, slot, posX, PosX),
        send( @box,x(PosX+Step)), %moves box to the right
        % send( @window,flush), %flushes
        send(PosX, plus, Step),
        send(Frame, minus, 1)).
:- pce_end_class.









