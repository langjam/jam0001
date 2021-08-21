% every cell that is not a {ellipse,edge,text} must be a rect
:- dynamic ellipse/2.
:- dynamic text/2.
:- dynamic edge/2.

rect(X,_):-
    cell(X,_),
    \+ellipse(X,_),
    \+text(X,_),
    \+edge(X,_),
    % skip drawing or root
    \+toplevel(X,_).

toplevel(X,_):-drawing(X,_).

toplevel(X,_):-root(X,_).

root(id1,_).
    
drawing("0",_).
    
writeRect(X,_):-format('rect(~w,"").~n',[X]). 

listRects(S):-
    setof(R,rect(R,_),S).
    
printRects:-
    listRects(S),
    printRect(S).

printRect([]).
printRect([First|Rest]):-writeRect(First,_),printRect(Rest).

