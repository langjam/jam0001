portdirection([P,'input']):-
    ellipse(P,_),
    fillColor(P,"green").
portdirection([P,'output']):-
    ellipse(P,_),
    fillColor(P,"yellow").
allDirections(Bag):-
    bagof(Pair,portdirection(Pair),Bag).

printDirection([]).
printDirection([H|T]):-
    format("portdirection(~w,~w).~n", H),
    printDirection(T).

printAllDirections:-
    allDirections(Bag),
    printDirection(Bag).
printAllDirections.
