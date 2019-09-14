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
    transl(f(c(memb, [v('E'), l([v('E')]/v('_'))])), T),
    writeln(T).
*/
/*
testz :-
    source_pl_source_nl(
        `memb(E,[E|_]).`,
        `memb E _0 and
          _0 holds list E _1 .
        `
    ).
*/

/*
testz :-
    transl(r(c(goal,[v('R')]),[c(add,[c(s,[c(s,[n(0)])]),c(s,[c(s,[n(0)])]),v('R')])]),T),
    writeln(T).
*/
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
/*
testz :-
    zterm(c(memb, [v('E'), l([v('E')]/v('_'))]),[],T,Vs),
    writeln(T-Vs).
*/

testz :-
    transl(f(c(a,[n(1)])),T),
    writeln(T).

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
%   top level rewrite interface, takes either a fact or a rule
%
transl(f(Pl),Nl) :-
    %zterm(Pl,[],Nl,_Vt).
    tterm(Pl,[],Nl,_Vt).
transl(r(H,Bs),Nl) :-
    zterm(H,[],Ht,Vh),
    zargs3(Bs,Vh,Bt,_Vt),
    append([Ht,[if],Bt],Nl).

tterm(c(F,As),Vs,Translated,Us) :-
    targs(As,Vs,A1s,A2s,Us),
    conj([F|A1s],and,A2s,Translated).
/*
tterm(l(H/T),Vs,Translated,Us) :-
    zargs1(H,Vs,_,V1),
    zargs1([T],V1,_,V2),
    append(H,[T],L),
    zargs2(L,V2,Translated,Us).
*/
tterm(n(N),Vs,[N],Vs).
tterm(v(V),Vs,[V],Vs).

targs([],Vs,[],[],Vs).
targs([A|As],Vs,[A1t|A1ts],[A2t|A2ts],Zs) :-
    hterm(A,Vs,A1t,A2t,Us),
    targs(As,Us,A1ts,A2ts,Zs).

hterm(T,Vs,G,[G,holds|Bl],Zs) :-
    genvar(Vs,Us,G),
    tterm(T,Us,Bl,Zs).
hterm(n(N),Vs,n(N),[],Vs).
hterm(v(V),Vs,v(V),[],Vs).

%!  zterm(+Term,+Vars,-Translated,-VarsUpdated) is det
zterm(c(F,As),Vs,Translated,Us) :-
    zargs1(As,Vs,A1s,Gs),
    zargs2(As,Vs,A2s,Us),
    assertion(Gs==Us),
    conj([F|A1s],and,A2s,Translated).
zterm(l(H/T),Vs,Translated,Us) :-
    zargs1(H,Vs,_,V1),
    zargs1([T],V1,_,V2),
    append(H,[T],L),
    zargs2(L,V2,Translated,Us).
zterm(n(N),Vs,[N],Vs).
zterm(v(V),Vs,[V],Vs).

%!  zargs1(+Term,+Vars,-VarsAllocated) is det
%
zargs1([],Vs,[],Vs).
zargs1([c(_,Qs)|As],Vs,[B|Bs],Vt) :-
    genvar(Vs,Vu,B),
    zargs1(Qs,Vu,_,Vr),
    zargs1(As,Vr,Bs,Vt).
zargs1([l(H/T)|As],Vs,[B|Bs],Vt) :-
    genvar(Vs,V1,B),
    zlist(H,T,L),
    zargs1(L,V1,_,V2),
    zargs1(As,V2,Bs,Vt).
zargs1([v('_')|As],Vs,[V|Bs],Vt) :-
    genvar(Vs,Vu,V),
    zargs1(As,Vu,Bs,Vt).
zargs1([v(V)|As],Vs,[V|Bs],Vt) :-
    zargs1(As,Vs,Bs,Vt).
zargs1([n(N)|As],Vs,[N|Bs],Vt) :-
    zargs1(As,Vs,Bs,Vt).

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
    zargs2(L,Ut,W,Us),
    zargs2(As,Us,Cs,Vu),
    conj([V,holds,list|W],and,Cs,Ts).
zargs2([_|As],Vs,Ts,Vu) :-
    zargs2(As,Vs,Ts,Vu).

zargs3([],Vs,[],Vs).
zargs3([B|Bs],Vh,Ts,Vu) :-
    zterm(B,Vh,Bt,Vn),
    zargs3(Bs,Vn,Rs,Vu),
    conj(Bt,and,Rs,Ts).

zlist(H,nil,H).
zlist(H,T,L) :- append(H,[T],L).

vlist([],Vs,[],Vs).
vlist([H|T],V1,Lj,V3) :-
    zterm(H,V1,Ht,V2),
    vlist(T,V2,Lr,V3),
    append(Ht,Lr,Lj).

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
/*
source_pl_source_nl(PlSource,NlSource) :-
    phrase(nl_source(NlSource),NlClause),
    source_nl_clause(PlSource,NlClause).
*/
source_pl_source_nl(PlSource,NlSource) :-
    phrase(pl_source([PlClause]),PlSource),!,
    transl(PlClause,Translated),
    phrase(nl_source(NlClause),NlSource),
    Translated == NlClause.

%!  nl_source(-Parsed)// is det
%
nl_source([]) --> s, ".", s.
nl_source([T|Ts]) --> nl_token(T), s, nl_source(Ts).
nl_token(T) --> name(T).
nl_token(T) --> var(T).
nl_token(T) --> number(T).

:- begin_tests(boot_pl2nl).

test(mini_fact) :-
    source_clause(
        `a(1).`,
        f(c(a,[n(1)]))
    ).

test(mini_goal) :-
    source_clause(
        `goal(Y):-a(Y).`,
        r(c(goal, [v('Y')]), [c(a, [v('Y')])])
    ).

test(mini_fact_pl_nl) :-
    source_pl_source_nl(
        `a(1).`,
        `a 1 .`
    ).
test(mini_goal_pl_nl) :-
    source_pl_source_nl(
        `goal(Y):-a(Y).`,
        `goal Y
         if
          a Y .`
    ).

test(memb_base_pl_nl) :-
    source_pl_source_nl(
        `memb(E,[E|_]).`,
        `memb E _0 and
          _0 holds list E _1 .
        `
    ).
/*
test(memb_loop) :-
    source_clause(
        `memb(E,[_|T]) :- memb(E,T).`,
        r(c(memb, [v('E'), l([v('_')]/v('T'))]), [c(memb, [v('E'), v('T')])])
    ).
test(memb_loop_pl_nl) :-
    source_pl_source_nl(
        `memb(E,[_|T]) :- memb(E,T).`,
        `memb E _0 and
           _0 holds list _1 T
         if
           memb E T .`
    ).
test(memb_goal_pl_nl) :-
    source_pl_source_nl(
        `goal(E):-memb(E,[0,1,2,3]).`,
        `goal E
         if
          memb E _0 and
          _0 lists 0 1 2 3 .
        `
    ).
test(add_base) :-
    source_clause(
        `add(0,X,X).`,
        f(c(add,[n(0),v('X'),v('X')]))
    ).
test(add_base_pl_nl) :-
    source_pl_source_nl(
        `add(0,X,X).`,
        `add 0 X X .`
    ).
test(add_loop_pl_nl) :-
    source_pl_source_nl(
        `add(s(X),Y,s(Z)):-add(X,Y,Z).`,
        `add _0 Y _1 and
         _0 holds s X and
         _1 holds s Z
        if
         add X Y Z .`
    ).
test(add_goal) :-
    source_clause(
        `goal(R):-add(s(s(0)),s(s(0)),R).`,
        r(c(goal,[v('R')]),[c(add,[c(s,[c(s,[n(0)])]),c(s,[c(s,[n(0)])]),v('R')])])
    ).
test(add_goal_pl_nl) :-
    source_clause(
        `goal(R):-add(s(s(0)),s(s(0)),R).`,
        `goal R
         if
          add _0 _1 R and
          _0 holds s _2 and
          _2 holds s 0 and
          _1 holds s _3 and
          _3 holds s 0 .`
    ).
*/
:- end_tests(boot_pl2nl).
