/*  File:    boot_pl2nl.pl
    Author:  Carlo,,,
    Created: Nov  7 2018
    Purpose: bypass pl2nl.pl, directly parsing prolog fragments to
    pseudo natural hhprolog loadable assembly
*/

:- module(boot_pl2nl, [
              boot_pl2nl/1, boot_pl2nl/2,
              pl2nl//1, rule//1, callable//1,transl/2,tterm//3,targs//3
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

tterm(s(F,As),V0s,Vs) --> atom(F), " ", targs(As,V0s,Vs).
tterm(n(N), Vs,Vs) --> number(N).
tterm(a(A), Vs,Vs) --> atom(A).
tterm(v(V), Vs,Vs) --> atom(V).

tterm(l(nil), Vs,Vs) --> "nil".
tterm(l(H/nil), V0s,Vs) --> "lists ", tlist(H, V0s,Vs).
tterm(l(H/T), V0s,Vs) --> "list ", tlist(H,V0s,V1s), tterm(T,V1s,Vs).

targs([A|As], V0s,Vs) --> tterm(A,V0s,V1s), " ", targs(As,V1s,Vs).
targs([], Vs,Vs) --> [].

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
    ctype([csymf, lower], C),
    cstype(csym, Cs),
    {atom_codes(N,[C|Cs])}.

var(V) -->
    ctype(prolog_var_start,C),
    cstype(prolog_identifier_continue, Cs),
    {atom_codes(V,[C|Cs])}.

ctype(T, C) --> [C], {is_list(T) -> maplist(code_type(C),T) ; code_type(C,T)}.

cstype(Type, [C|Cs]) --> [C], {code_type(C,Type)}, cstype(Type, Cs).
cstype(_, []) --> [].

% debug
deb(X) --> {writeln(+X)}.
