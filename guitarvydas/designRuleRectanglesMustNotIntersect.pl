designRuleRectanglesMustNotIntersect(A,B):-
    % simplify by checking bounding boxes only
    rect(A,_),
    rect(B,_),
    l(A,Al),
    t(A,At),
    r(A,Ar),
    %b(A,Ab),
    l(B,Bl),
    t(B,Bt),
    %r(B,Br),
    b(B,Bb),
    linesIntersect(Al, At, Ar, At, Bl, Bt, Bl, Bb),
    format('FATAL design rule: rectangles must not intersect (Atop Bleft) ~w ~w~n', [A,B]),
    !.

designRuleRectanglesMustNotIntersect(A,B):-
    % simplify by checking bounding boxes only
    rect(A,_),
    rect(B,_),
    l(A,Al),
    t(A,At),
    r(A,Ar),
    %b(A,Ab),
    %l(B,Bl),
    t(B,Bt),
    r(B,Br),
    b(B,Bb),
    linesIntersect(Al, At, Ar, At, Br, Bt, Br, Bb),
    format('FATAL design rule: rectangles must not intersect (Atop Bright) ~w ~w~n', [A,B]),
    !.


designRuleRectanglesMustNotIntersect(A,B):-
    % simplify by checking bounding boxes only
    rect(A,_),
    rect(B,_),
    l(A,Al),
    %t(A,At),
    r(A,Ar),
    b(A,Ab),
    l(B,Bl),
    t(B,Bt),
    %r(B,Br),
    b(B,Bb),
    linesIntersect(Al, Ab, Ar, Ab, Bl, Bt, Bl, Bb),
    format('FATAL design rule: rectangles must not intersect (Abottom Bleft)~n', [A,B]),
    !.

designRuleRectanglesMustNotIntersect(A,B):-
    % simplify by checking bounding boxes only
    rect(A,_),
    rect(B,_),
    l(A,Al),
    %t(A,At),
    r(A,Ar),
    b(A,Ab),
    %l(B,Bl),
    t(B,Bt),
    r(B,Br),
    b(B,Bb),
    linesIntersect(Al, Ab, Ar, Ab, Br, Bt, Br, Bb),
    format('FATAL design rule: rectangles must not intersect (Abottom Bright) ~w ~w~n', [A,B]),
    !.

designRuleRectanglesMustNotIntersect(A,B):- A = B.
designRuleRectanglesMustNotIntersect(A,B):- A \= B.


linesIntersect(Xa1, Ya1, Xa2, Ya2, Xb1, Yb1, Xb2, Yb2):-
    linesIntersectAisVertical(Xa1, Ya1, Xa2, Ya2, Xb1, Yb1, Xb2, Yb2).
linesIntersect(Xa1, Ya1, Xa2, Ya2, Xb1, Yb1, Xb2, Yb2):-
    linesIntersectAisVertical(Xb1, Yb1, Xb2, Yb2, Xa1, Ya1, Xa2, Ya2).

linesIntersectAisVertical(Xa1, Ya1, Xa2, Ya2, Xb1, Yb1, Xb2, Yb2):-
    % perpendicular lines only
    % A is vertical (hence, B is horizonal)
    Yb1 >= Ya1,
    Yb2 =< Ya2,
    Xb1 =< Xa1,
    Xb2 >= Xa2.

designRuleRectanglesMustNotIntersectOnTheSameDiagram:-
    bagof([A,B],(rect(A,""),rect(B,""),designRuleRectanglesMustNotIntersectOnTheSameDiagram(A,B)),_),!.

designRuleRectanglesMustNotIntersectOnTheSameDiagram(A,B):-
    A \= B,
    onSameDiagram(A,B),
    designRuleRectanglesMustNotIntersect(A,B).
