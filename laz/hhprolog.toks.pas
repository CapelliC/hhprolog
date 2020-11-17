(*
 * hhprolog: Hitchhiker Prolog
 *
 * Version: 1.0.0
 * License: MIT
 *
 * Copyright (c) 2018,2019,2020 Carlo Capelli
 *)

unit hhprolog.Toks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, RegExpr;

type

  tok = record
    t, s: string;
    n: integer;
    //class operator initialize(var aTok:Tok);
  end;

  TToks = specialize TList<tok>;

  Ts = TStringList;
  //Tss = specialize TList<Ts>;
  //Tsss = specialize TList<Tss>;
  Tss = specialize TObjectList<Ts>;
  Tsss = specialize TObjectList<Tss>;

  //pTss = ^Tss;

const
  // tokens as regex specification
  SPACE = '\s+';
  ATOM  = '[a-z]\w*';
  &VAR  = '[A-Z_]\w*';
  NUM   = '-?\d+';
  DOT   = '\.';

  // atom keywords
  &IF   = 'if';
  &AND  = 'and';
  HOLDS = 'holds';
//NIL   = 'nil',
  LISTS = 'lists';
  &IS   = 'is';  // ?

  N_BITS_INT = 28;

function makeToks(s: string):  TToks;

function toSentences(s: string): Tsss;
function maybeExpand(Ws: Ts): Tss;
function mapExpand(Wss: Tss) : Tss;

function SubStr(s: string; start: integer; count: integer = -1): string; inline;

implementation

function clone(Ws: Ts): Ts;
  begin
    result := Ts.create;
    result.assign(Ws)
  end;

{
class operator Tok.initialize(var aTok:Tok);
begin
  result.t := t;
  result.s := s;
  result.n := n;
end;
}

function SubStr(s: string; start: integer; count: integer): string;
begin
  if count = -1 then
    count := Length(s) - start;
  Exit(Copy(s, start + 1, count))
end;

function makeToks(s: string):  TToks;
  var re: TRegExpr; t: tok;
  function token: tok;
    function tsn(t, s: string; n: integer = 0): tok;
    begin
      result.t := t;
      result.s := s;
      result.n := n;
    end;
  begin
    if re.MatchLen[1] > 0 then
       Exit(tsn(SPACE, re.Match[1]));
    if re.MatchLen[2] > 0 then begin
       case re.Match[2] of
         &IF, &AND, HOLDS, LISTS, &IS:
           Exit(tsn(re.Match[2], re.Match[2]))
       end;
       Exit(tsn(ATOM, re.Match[2]))
    end;
    if re.MatchLen[3] > 0 then
       Exit(tsn(&VAR, re.Match[3]));
    if re.MatchLen[4] > 0 then
       Exit(tsn(NUM, re.Match[4], StrToInt(re.Match[4])));
    if re.MatchLen[5] > 0 then
       Exit(tsn(DOT, re.Match[5]));
    raise Exception.Create('no match')
  end;
begin
  re := TRegExpr.Create('('+SPACE+')|('+ATOM+')|('+&VAR+')|('+NUM+')|('+DOT+')');
  result := TToks.Create;
  if re.Exec(s) then begin
     repeat
       t := token;
       if t.t <> SPACE then
          result.Add(t)
     until not re.ExecNext
  end;
  FreeAndNil(re)
end;

function toSentences(s: string): Tsss;
  var
    Wsss: Tsss;
    Wss: Tss;
    Ws: Ts;

    t: tok;
    lt: TToks;
begin
  Wsss := Tsss.Create;
  Wss := Tss.Create;
  Ws := Ts.Create;

  lt := makeToks(s);
  for t in lt do
  begin
    case t.t of
      DOT: begin
	   	  Wss.Add(Ws); Ws := Ts.Create;
	   	  Wsss.Add(Wss); Wss := Tss.Create
	      end;
      &IF, &AND: begin
				Wss.Add(Ws); Ws := Ts.Create
				end;
      HOLDS:
        Ws[0] := 'h:' + SubStr(Ws[0], 2);
      LISTS:
				Ws[0] := 'l:' + SubStr(Ws[0], 2);
      &IS:
				Ws[0] := 'f:' + SubStr(Ws[0], 2);
      &VAR:
        Ws.Add('v:' + t.s);
      NUM:
        if t.n < (1 << N_BITS_INT) then
          Ws.Add('n:' + t.s)
        else
          Ws.Add('c:' + t.s);
      ATOM:
        Ws.Add('c:' + t.s);
      else
        raise Exception.Create('unknown token:' + t.t);
    end;
	end;
  lt.Free; Wss.Free; Ws.Free;
  exit(Wsss)
end;

(**
 * expands a "Xs lists .." statements to "Xs holds" statements
 *)
function maybeExpand(Ws: Ts): Tss;
  var
    W, V, Vi, Vii: string;
    n, i: integer;
    Rs: Ts;
begin
  result := Tss.Create;
  W := Ws[0];
  if (Length(W) < 2) or ('l:' <> SubStr(W, 0, 2)) then
    exit;
  n := Ws.Count;
  V := SubStr(W, 2);
  Rs := Ts.Create;
  for i := 1 to n - 1 do
  begin
    if 1 = i then
      Vi := V
    else
      Vi := V + '__' + IntToStr(i - 1);
    Vii := V + '__' + IntToStr(i);
    if i = n - 1 then
      Rs.AddStrings(['h:' + Vi, 'c:list', Ws[i], 'c:nil'])
    else
      Rs.AddStrings(['h:' + Vi, 'c:list', Ws[i], 'v:' + Vii]);
    result.Add(Rs);
    Rs := Ts.Create;
	end;
  Rs.Free;
end;

(**
 * expands, if needed, "lists" statements in sequence of statements
 *)
function mapExpand(Wss: Tss) : Tss;
  var Hss: Tss;
      Ws, Cs: Ts;
begin
  result := Tss.Create;
  for Ws in Wss do
  begin
    Hss := maybeExpand(Ws);
    if Hss.Count = 0 then
      result.Add(clone(Ws))
    else
      for Cs in Hss do
        result.Add(clone(Cs));
    Hss.Free
	end;
end;

end.

