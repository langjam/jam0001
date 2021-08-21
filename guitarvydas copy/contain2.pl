:- dynamic contains1/2.

deepcontains1(Parent,GrandChild):-
    onSameDiagram(Parent,GrandChild),
    contains1(Parent,GrandChild),
    contains1(Child,GrandChild),
    contains1(Parent,Child).
deepcontains1(Parent,GrandChild):-
    onSameDiagram(Parent,GrandChild),
    contains1(Parent,GrandChild),
    contains1(Child,GrandChild),
    deepcontains1(Parent,Child).

deepcontains(R,Bag):-
    setof(X,deepcontains1(R,X),Bag).

printContains(R):-
    deepcontains(R,Bag),
    printContainsSingle(R,Bag).

printContainsSingle(_,[]).
printContainsSingle(R,[H|T]):-
    format("deepcontains(~w,~w).~n", [R,H]),
    printContainsSingle(R,T).

printAllDeepContains:-
    bagof(R,(rect(R,_),printContains(R)),_).
printAllDeepContains.
