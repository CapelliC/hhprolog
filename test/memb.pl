memb(E,[E|_]).
memb(E,[_|T]) :- memb(E,T).

goal(E):-memb(E,[0,1,2,3,4,5,6,7,8,9,10,11]).
