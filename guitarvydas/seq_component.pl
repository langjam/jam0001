component(Diagram,C,Name,Ins,Outs,"",Children,Connections) :-
    diagramContains(Diagram,C),
    rect(C,_),
    cname(C,Name),
    inputsof(C,Ins),
    outputsof(C,Outs),
    childrenOf(C,Children),
    connectionsOf(C,Connections).

inputsof(C,InBag):-
    inputof(C,_),
    bagof(I,inputof(C,I),InBag),
    !.
inputsof(_,[]).

outputsof(C,InBag):-
    outputof(C,_),
    bagof(I,outputof(C,I),InBag),
    !.
outputsof(_,[]).

    

childrenOf(C,Children):-
    childof(C,_),
    bagof(Child,childof(C,Child),Children),!.
childrenOf(_,[]).

connectionsOf(C,Connections):-
    connectionOf(C,_),
    bagof(Conn,connectionOf(C,Conn),Connections),!.
connectionsOf(_,[]).
    

toplevelComponent(Diagram,C):-
    diagramContains(Diagram,C),
    rect(C,_),
    value(C,_),
    \+ contains(_,C).

childComponent(Diagram,C):-
    diagramContains(Diagram,C),
    rect(C,_),
    value(C,_).

alltoplevelComponentsOnDiagram(Diagram,Bag):-
    setof(C,toplevelComponent(Diagram,C),Bag).

allchildrenComponents(C,Bag):-
    setof(Child,contains(C,Child),Bag).

inputof(C,Name):-
    ellipse(I,_),
    contains(C,I),
    fillColor(I,"green"),
    cname(I,Name).

outputof(C,Name):-
    ellipse(O,_),
    contains(C,O),
    fillColor(O,"yellow"),
    cname(O,Name).

childof(C,Name):-
    contains(C,Child),
    rect(Child,_),
    cname(Child,Name).

connectionOf(C,connection{name:ConnectionName,source:pair{component:SourceName,port:SourcePort},target:pair{component:TargetName,port:TargetPort}}):-
    contains(C,E),
    edge(E,_),
    source(E,SC),
    cname(SC,SourcePort),
    contains(SourceParent,SC),
    getname(C,SourceParent,SourceName),
    target(E,TC),
    cname(TC,TargetPort),
    contains(TargetParent,TC),
    getname(C,TargetParent,TargetName),
    gensym(x,ConnectionName).
    
cname(C,Name):-value(C,Name).
cname(_,C,Name):-cname(C,Name).

getname(Parent,Child,Name):-
    Child \= Parent,
    cname(Child,Name),
    !.
getname(_,_,self).
