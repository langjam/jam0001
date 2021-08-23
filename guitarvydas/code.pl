
makecomponentcode(C):-
    fillColor(C,"red"),
    value(C,Code),
    format("factcomponentcode(~w,\"~w\").~n",[C,Code]),!.
makecomponentcode(C):-
    format("factcomponentcode(~w,\"\").~n",[C]).


getcode(Parent,Child,Code):-
    Child \= Parent,
    factcomponentcode(Child,Code),
    !.
getcode(_,_,self).

componentcode(C,Code):-
    factcomponentcode(C,Code).




printCode:-
    forall( component(C),
	    makecomponentcode(C)
	  ).
