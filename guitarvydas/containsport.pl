containsport(R,C):-
    rightsideinsideBB(C,R).
containsport(R,C):-
    leftsideinsideBB(C,R).
containsport(R,C):-
    bottomsideinsideBB(C,R).
containsport(R,C):-
    topsideinsideBB(C,R).

rightsideinsideBB(Circle,Rect):-
    rect(Rect,_),
    ellipse(Circle,_),
    onSameDiagram(Rect,Circle),
    l(Rect,Rl),   t(Rect,Rt),   r(Rect,Rr),   b(Rect,Rb),
    l(Circle,Cl), t(Circle,Ct), r(Circle,Cr), b(Circle,Cb),
    Cl =< Rl,
    Ct >= Rt, Ct =< Rb,
    Cr >= Rl, Cr =< Rr,
    Cb >= Rt, Cb =< Rb.
leftsideinsideBB(Circle,Rect):-
    rect(Rect,_),
    ellipse(Circle,_),
    onSameDiagram(Rect,Circle),
    l(Rect,Rl),   t(Rect,Rt),   r(Rect,Rr),   b(Rect,Rb),
    l(Circle,Cl), t(Circle,Ct), r(Circle,Cr), b(Circle,Cb),
    Cl =< Rr, Cl >= Rl,
    Ct >= Rt, Ct =< Rb,
    Cr >= Rr,
    Cb >= Rt, Cb =< Rb.
bottomsideinsideBB(Circle,Rect):-
    rect(Rect,_),
    ellipse(Circle,_),
    onSameDiagram(Rect,Circle),
    l(Rect,Rl),   t(Rect,Rt),   r(Rect,Rr),   b(Rect,Rb),
    l(Circle,Cl), t(Circle,Ct), r(Circle,Cr), b(Circle,Cb),
    Cl >= Rl, Cl =< Rr,
    Ct =< Rt,
    Cr >= Rl, Cr =< Rr,
    Cb >= Rt, Cb =< Rb.
topsideinsideBB(Circle,Rect):-
    rect(Rect,_),
    ellipse(Circle,_),
    onSameDiagram(Rect,Circle),
    l(Rect,Rl),   t(Rect,Rt),   r(Rect,Rr),   b(Rect,Rb),
    l(Circle,Cl), t(Circle,Ct), r(Circle,Cr), b(Circle,Cb),
    Cl >= Rl, Cl =< Rr,
    Ct >= Rt, Ct =< Rb,
    Cr >= Rl, Cr =< Rr,
    Cb >= Rb.

allPortContains(B):-
    setof([R,C],containsport(R,C),B).

printAllPortContains:-
    allPortContains(B),
    printPortContains(B).
printAllPortContains.

printPortContains([]).
printPortContains([H|T]) :-
    format("contains(~w,~w).~n",H),
    printPortContains(T).
