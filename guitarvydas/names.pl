
makecomponentname(C):-
    fillColor(C,"red"),
    gensym(c,Name),
    format("factcomponentname(~w,\"~w\").~n",[C,Name]),
    !.
makecomponentname(C):-
    value(C,Name),
    format("factcomponentname(~w,\"~w\").~n",[C,Name]),!.
makecomponentname(C):-
    gensym(c,Name),
    format("unknowncomponentname(~w,\"~w\").~n",[C,Name]).

getname(Parent,Child,Name):-
    Child \= Parent,
    factcomponentname(Child,Name),
    !.
getname(_,_,self).

componentname(C,Name):-
    factcomponentname(C,Name).




printNames:-
    forall( component(C),
	    makecomponentname(C)
	  ).
