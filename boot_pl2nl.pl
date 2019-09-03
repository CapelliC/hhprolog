/*  File:    boot_pl2nl.pl
    Author:  Carlo,,,
    Created: Nov  7 2018
    Purpose: bypass pl2nl.pl, directly parsing prolog clauses to
             pseudo natural hhprolog loadable assembly
*/

:- module(boot_pl2nl,
          [pl_source//1
          ,transl/2
          ,testz/0
          ,make_test/2
          ,boot_pl2nl/1
          ,boot_pl2nl/2
          ]).

:- use_module(library(debug)).
:- use_module(library(plunit)).
:- use_module(library(dcg/basics)).

/*
testz :-
    transl(r(c(goal,[v('E')]),[c(memb,[v('E'),l([n(0),n(1),n(2),n(3)]/nil)])]),T),
    writeln(T).
*/
/*
testz :-
    source_pl_source_nl(
        `goal(E):-memb(E,[0,1,2,3]).`,
        `goal E
         if
          memb E _0 and
          _0 lists 0 1 2 3 .
        `
    ).
*/
testz :-
    zterm(c(memb, [v('E'), l([v('E')]/v('_'))]),[],T,Vs),
    writeln(T-Vs).
/*
testz :-
    zterm(c(memb, [v('E'), l([n(0), n(1), n(2)]/nil)]),[],T,Vt),
    writeln(T-Vt).
*/
/*
testz :-
    zargs1([v('E'), l([v('E')]/v('_'))], [], As),
    writeln(As).
*/
/*
testz :-
    PlSource = `memb(E,[E|_]).`,
    phrase(pl_source([Pl]),PlSource),
    NlSource = `memb E _0 and _0 holds list E _1 .`,
    phrase(nl_source(NlExpect),NlSource),
    transl(Pl,Tr),
    Tr==NlExpect.
*/
boot_pl2nl(F) :-
    phrase_from_file(pl_source(Pl),F),
    maplist(writeln,Pl), !,
    nl,
    maplist(transl,Pl,Nl),
    maplist([S]>>format('~s.~n',[S]),Nl).

boot_pl2nl(Pl,Nl) :-
    phrase(pl_source(Nl),Pl).

%!  transl(+Clause,-Linearized) is det
%
transl(f(Pl),Nl) :-
    zterm(Pl,[],Nl,_Vt).
transl(r(H,Bs),Nl) :-
    zterm(H,[],Ht,Vh),
    zargs3(Bs,Vh,Bt,_Vt),
    append([Ht,[if],Bt],Nl).

%!  zterm(+Term,+Vars,-Translated,-VarsUpdated) is det
zterm(c(F,As),Vs,Translated,Us) :-
    zargs1(As,Vs,A1s),
    zargs2(As,Vs,A2s,Us),
    conj([F|A1s],and,A2s,Translated).
zterm(l(H/T),Vs,Translated,Us) :-
    zargs1(H,Vs,V1),
    zargs1([T],V1,V2),
    append(H,[T],L),
    zargs2(L,V2,Translated,Us).
zterm(n(N),Vs,[N],Vs).
zterm(v(V),Vs,[V],Vs).

%!  zargs1(+Term,+Vars,-VarsAllocated) is det
%
zargs1([],_,[]).
zargs1([c(_,_)|As],Vs,[B|Bs]) :-
    genvar(Vs,Vu,B),
    zargs1(As,Vu,Bs).
zargs1([l(H/T)|As],Vs,[B|Bs]) :-
    genvar(Vs,V1,B),
    zlist(H,T,L),
    zargs1(L,V1,V2),
    zargs1(As,V2,Bs).
zargs1([v('_')|As],Vs,[V|Bs]) :-
    genvar(Vs,Vu,V),
    zargs1(As,Vu,Bs).
zargs1([v(V)|As],Vs,[V|Bs]) :-
    zargs1(As,Vs,Bs).
zargs1([n(_)|As],Vs,Bs) :-
    zargs1(As,Vs,Bs).

zlist(H,nil,H).
zlist(H,T,L) :- append(H,[T],L).

vlist([],Vs,[],Vs).
vlist([H|T],V1,Lj,V3) :-
    zterm(H,V1,Ht,V2),
    vlist(T,V2,Lr,V3),
    append(Ht,Lr,Lj).

%!  zargs2(+Arg,+Vars,-Flat,-VarsUpd) is det
%
zargs2([],Vs,[],Vs).
zargs2([c(F,Qs)|As],Vs,Ts,Vu) :-
    genvar(Vs,Ut,V),
    zterm(c(F,Qs),Ut,Bl,Us),
    zargs2(As,Us,Cs,Vu),
    conj([V,holds|Bl],and,Cs,Ts).
zargs2([l(H/nil)|As],Vs,Ts,Vu) :-
    genvar(Vs,V1,V),
    vlist(H,V1,Lt,V2),
    zargs2(As,V2,Cs,Vu),
    conj([V,lists|Lt],and,Cs,Ts).
zargs2([l(H/T)|As],Vs,Ts,Vu) :-
    genvar(Vs,Ut,V),
    zlist(H,T,L),
    zargs1(L,Ut,Us),
    zargs2(As,Us,Cs,Vu),
    conj([V,holds,list|Us],and,Cs,Ts).
zargs2([_|As],Vs,Ts,Vu) :-
    zargs2(As,Vs,Ts,Vu).

zargs3([],Vs,[],Vs).
zargs3([B|Bs],Vh,Ts,Vu) :-
    zterm(B,Vh,Bt,Vn),
    zargs3(Bs,Vn,Rs,Vu),
    conj(Bt,and,Rs,Ts).

%!  genvar(+VarsSoFar,-WithNewlyAllocated) is det
%
genvar(V0s,[V|V0s]) :-
    length(V0s,N),
    format(atom(V),'_~d',[N]).
genvar(V0s,[V|V0s],V) :-
    genvar(V0s,[V|V0s]).

%!  conj(+Left,+And,+Right,-Join) is det
%
conj(L,And,R,J) :-
    (   R = []
    ->  J = L
    ;   R = [And|_]
    ->  append([L,R],J)
    ;   append([L,[And],R],J)
    ).

%!  pl2nl(?Clauses)// is det
%
%   parsing a subset of pure Prolog
%
pl_source([P|Ps]) --> s, rule(P), s, pl_source(Ps).
pl_source([]) --> s.
pl_source(_) --> syntax_error('cannot parse').

s --> blank, s.
s --> comment, s.
s --> [].

comment --> "/*", string(_), "*/".
comment --> "%", string_without("\n", _).

rule(r(H,Bs)) --> callable(H), s, ":-", s, body(Bs), s, ".".
rule(f(F)) --> callable(F), s, ".".

callable(c(A,Ts)) --> funct(A), "(", s, terms(Ts), s, ")".
callable(c(A,[])) --> name(A).

body([G|Gs]) --> callable(G), body_(Gs).
body_(Gs) --> s, ",", s, body(Gs).
body_([]) --> [].

terms([T|Ts]) --> term(T), terms_(Ts).
terms_(Ts) --> s, ",", s, terms(Ts).
terms_([]) --> [].

term(n(N)) --> number(N).
term(l(L)) --> list(L).
term(v(V)) --> var(V).
term(C) --> callable(C).

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
deb(X) --> {debugging(boot_pl2nl) -> writeln(+X) ; true}.

make_test(T,S) :-
    phrase(pl_source([Pl]),S),
    format('test(~s) :-~n~4|source_clause(~n~8|`~s`,~n~8|~q~n~4|).', [T,S,Pl]).

source_clause(Source,Clause) :-
    phrase(pl_source(Nl),Source),!,
    Nl=[Clause].
source_nl_clause(Source,NlClause) :-
    source_clause(Source,Parsed),
    transl(Parsed,NlClause).

source_pl_source_nl(Source,NlClauseSource) :-
    phrase(nl_source(NlClause),NlClauseSource),
    source_nl_clause(Source,NlClause).

%!  nl_source(-Parsed)// is det
%
nl_source([]) --> s, ".", s.
nl_source([T|Ts]) --> nl_token(T), s, nl_source(Ts).
nl_token(T) --> name(T).
nl_token(T) --> var(T).
nl_token(T) --> number(T).

:- begin_tests(boot_pl2nl).

test(mini) :-
    source_clause(
        `a(1).`,
        f(c(a,[n(1)]))
    ).
test(mini) :-
    source_clause(
        `goal(Y):-a(Y).`,
        r(c(goal, [v('Y')]), [c(a, [v('Y')])])
    ).

test(memb_base) :-
    source_pl_source_nl(
        `memb(E,[E|_]).`,
        `memb E _0 and
          _0 holds list E _1 .
        `
    ).
test(memb_loop) :-
    source_clause(
        `memb(E,[_|T]) :- memb(E,T).`,
        r(c(memb, [v('E'), l([v('_')]/v('T'))]), [c(memb, [v('E'), v('T')])])
    ).
test(memb_goal) :-
    source_pl_source_nl(
        `goal(E):-memb(E,[0,1,2,3]).`,
        `goal E
         if
          memb E _0 and
          _0 lists 0 1 2 3 .
        `
    ).

test(add) :-
    source_clause(
        `add(0,X,X).`,
        f(c(add,[n(0),v('X'),v('X')]))
    ).
test(add) :-
    source_pl_source_nl(
        `add(s(X),Y,s(Z)):-add(X,Y,Z).`,
        `add _0 Y _1 and
         _0 holds s X and
         _1 holds s Z
        if
         add X Y Z .`
    ).
test(add) :-
    source_clause(
        `goal(R):-add(s(s(0)),s(s(0)),R).`,
        r(c(goal,[v('R')]),[c(add,[c(s,[c(s,[n(0)])]),c(s,[c(s,[n(0)])]),v('R')])])
    ).

:- end_tests(boot_pl2nl).
