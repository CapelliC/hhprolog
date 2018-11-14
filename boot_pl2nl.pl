/*  File:    boot_pl2nl.pl
    Author:  Carlo,,,
    Created: Nov  7 2018
    Purpose: bypass pl2nl.pl, directly parsing prolog fragments to
    pseudo natural hhprolog loadable assembly
*/

:- module(boot_pl2nl,
          [   boot_pl2nl/1
             ,boot_pl2nl/2
             ,tterm//3
              %pl2nl//1, rule//1, callable//1,transl/2,
              %targs//3
          ]).

:- use_module(library(dcg/basics)).

boot_pl2nl(F) :-
    phrase_from_file(pl2nl(Pl), F),
    maplist(writeln, Pl), !,
    nl,
    maplist(transl, Pl, Nl),
    maplist([S]>>format('~s~n',[S]), Nl).

boot_pl2nl(Pl, Nl) :-
    phrase(pl2nl(Nl), Pl).

% translating
%
transl(r(H,Gs), Nl) :- phrase((tterm(H,[],Vh), "if " , tbody(Gs,true,Vh), "."), Nl).
transl(f(T), Nl) :- phrase((tterm(T,[],_), "."), Nl).

tbody([G|Gs],First,Vh) --> and(First), tterm(G,Vh,V1), tbody(Gs,false,V1).
tbody([],_,_) --> [].

and(true) --> [].
and(false) --> " and ".

%tterm(s(F,As), V0s,Vs) --> atom(F), " ", targs(As,V0s,Vs).
% ?- S=s(add,[n(0),v('X'),v('X')]), phrase(tterm(S,[],Vs),T), format('<~s>~n', [T]).
% ?- S=s(add,[s(s(s,[v('X')])),v('Y'),s(s(s,[v('Z')]))]), phrase(tterm(S,[],Vs),T), format('<~s>~n', [T]).
tterm(s(F,As), V0s,Vs) -->
    atom(F), " ",
    targs(As,V0s,Vs,[],Bs),
    deb(s(F,As)->Bs),
    (   {Bs\=[]}
    ->  {reverse(Bs,Cs)},
        xargs(Cs)
    ;   []
    ).
tterm(n(N), Vs,Vs) --> number(N).
tterm(a(A), Vs,Vs) --> atom(A).
tterm(v(V), Vs,Vs) --> atom(V).

tterm(l(nil), Vs,Vs) --> "nil".
tterm(l(H/nil), V0s,Vs) --> "lists ", tlist(H, V0s,Vs).
tterm(l(H/T), V0s,Vs) --> deb(l(H/T)), "list ", tlist(H,V0s,V1s), tterm(T,V1s,Vs).

/*
targs([A|As], V0s,Vs) --> deb(targs(A)), tfterm(A,V0s,V1s), " ", targs(As,V1s,Vs).
targs([], Vs,Vs) --> [].
*/
targs([A|As], V0s,Vs, Bs,U) -->
    deb(targs1(A)),
    targ(A, V0s,V1s, Bs,U0), " ",
    targs(As, V1s,Vs, U0,U),
    deb(targs2(A,U)).
targs([], Vs,Vs, S,S) --> [].

targ(v(V), Vs,Vs, S,S) --> atom(V).
targ(n(N), Vs,Vs, S,S) --> number(N).
targ(s(Q), V0s,Vs, S,[V:s(F,As)|S]) -->
    deb(+s(Q,Vs)),
    {Q=..[F|As], Vs=[V|V0s]},
    genvar(V0s,[V|V0s]), atom(V).
targ(X, Vs,Vs, S,S) -->
    deb(why(X)).

genvar(V0s,[V|V0s]) -->
    {length(V0s,N), format(atom(V),'_~d',[N])}.

xargs([V:E|Es]) --> xarg(V,E), xargs(Es).
xargs([]) --> [].

xarg(V,E) --> " and ", atom(V), " holds ", xarg(E).

xarg(s(F,_As)) --> atom(F).
xarg(n(N)) --> number(N).
xarg(v(V)) --> atom(V).

tfterm(v(V), Vs,Vs) --> atom(V).
tfterm(n(N), Vs,Vs) --> number(N).
tfterm(l(H/T), V0s,Vs) -->
    addvar(V0s,V1s), "list ", tlist(H,V0s,V1s), tterm(T,V1s,Vs).

addvar(V0s,[V|V0s]) -->
    {length(V0s,N), format(atom(V),'_~d',[N])},
    atom(V), " and ", atom(V), " holds ", deb(addvar(V0s, V)).


tlist([], Vs,Vs) --> [].
tlist([H|T], V0s,Vs) --> tterm(H,V0s,V1s), " ", tlist(T,V1s,Vs).

% parsing
%
pl2nl([P|Ps]) --> s, rule(P), deb(rule(P)), s, pl2nl(Ps).
pl2nl([]) --> s.
pl2nl(_) --> syntax_error('cannot parse').

s --> blank, s.
s --> comment, s.
s --> [].

comment --> "/*", deb(b1), string(_), "*/", deb(b2).
comment --> "%", deb(c1), string_without("\n", _), deb(c2).

rule(r(H,Bs)) --> callable(H), s, ":-", s, body(Bs), s, ".".
rule(f(F)) --> callable(F), s, ".".

callable(s(A,Ts)) --> funct(A), "(", s, terms(Ts), s, ")".
callable(s(A)) --> name(A).

body([G|Gs]) --> callable(G), deb(callable(G)), body_(Gs).
body_(Gs) --> s, ",", s, body(Gs).
body_([]) --> [].

terms([T|Ts]) --> term(T), /*deb(term(T)),*/ terms_(Ts).
terms_(Ts) --> s, ",", s, terms(Ts).
terms_([]) --> [].

term(n(N)) --> number(N).
%term(a(A)) --> name(A).
term(l(L)) --> list(L).
term(v(V)) --> var(V).
term(s(S)) --> callable(S).

list(Ts/nil) --> s, "[", s, terms(Ts), s, "]".
list(Ts/T) --> s, "[", s, terms(Ts), s, "|", s, term(T), s, "]".
list(nil) --> s, "[", s, "]".

funct(F) --> name(F).

name(N) -->
    typesc([csymf,lower], C),
    typescs([csym], Cs),
    {atom_codes(N,[C|Cs])}.

var(V) -->
    typesc([prolog_var_start],C),
    typescs([prolog_identifier_continue], Cs),
    {atom_codes(V,[C|Cs])}.

typesc(T, C) --> [C], {maplist(code_type(C),T)}.
typescs(T, [C|Cs]) --> typesc(T,C), typescs(T,Cs).
typescs(_, []) --> [].

% debug
deb(X) --> {writeln(+X)}.
