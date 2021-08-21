% create bounding box (Left, Top, Right, Bottom rectangle) for every vertex cell

makebb(CellID):-
    vertex(CellID,_),
    x(CellID,X),
    y(CellID,Y),
    width(CellID,Width),
    height(CellID,Height),
    format("l(~w,~w).~n",[CellID,X]),
    format("t(~w,~w).~n",[CellID,Y]),
    R is X + Width,
    format("r(~w,~w).~n",[CellID,R]),
    Bottom is Y + Height,
    format("b(~w,~w).~n",[CellID,Bottom]).

% this is not definitive, but, just enough to allow containment to deal with edges
makebb(Edge) :-
    edge(Edge,_),
    source(Edge,Source),
    vertex(Source,_),
    target(Edge,Target),
    vertex(Target,_),
    x(Source,Sx),
    y(Source,Sy),
    width(Source,Sw),
    height(Source,Sh),
    x(Target,Tx),
    y(Target,Ty),
    width(Source,Tw),
    height(Source,Th),
    % ports (ellipses) intersect the border of components (rectangles)
    % use the center-point of ports for LTRB
    Scx is (Sx + (Sw / 2)),
    Tcx is (Tx + (Tw / 2)),
    Scy is (Sy + (Sh / 2)),
    Tcy is (Ty + (Th / 2)),
    Right is max(Scx,Tcx),
    Left is min(Tcx,Scx),
    Bottom is max(Scy,Tcy),
    Top is min(Tcy,Scy),
    format("l(~w,~w).~n",[Edge,Left]),
    format("r(~w,~w).~n",[Edge,Right]),
    format("t(~w,~w).~n",[Edge,Top]),
    format("b(~w,~w).~n",[Edge,Bottom]).
    

printBB:-
    bagof(C,makebb(C),_).
