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
  {
  Tss = specialize TList<Ts>;
  Tsss = specialize TList<Tss>;
  }

  { Tss }

  Tss = class(specialize TList<Ts>)
    destructor Destroy; override;
  end;

  { Tsss }

  Tsss = class(specialize TList<Tss>)
    destructor Destroy; override;
  end;

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

function makeToks(s: string):  TToks;

function toSentences(s: string): Tsss;
function maybeExpand(Ws: Ts): Tss;
function mapExpand(Wss: Tss) : Tss;

function SubStr(s: string; start: integer; count: integer = -1): string; inline;

implementation

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
        if t.n < (1 << 28) then
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

function maybeExpand(Ws: Ts): Tss;
  var
    Rss: Tss;
    W, V, Vi, Vii: string;
    l, i: integer;
    Rs: Ts;
begin
  Rss := Tss.Create;
  W := Ws[0];
  if (Length(W) < 2) or ('l:' <> SubStr(W, 0, 2)) then
    exit(Rss);
  l := Ws.Count;
  V := SubStr(W, 2);
  Rs := Ts.Create;
  for i := 1 to l do
  begin
      if 1 = i then
        Vi := V
      else
        Vi := V + '__' + IntToStr(i - 1);
      Vii := V + '__' + IntToStr(i);
      Rs.Add('h:' + Vi); Rs.Add('c:list'); Rs.Add(Ws[i]);
      if i = l - 1 then
        Rs.Add('c:nil')
      else
        Rs.Add('v:' + Vii);
      Rss.Add(Rs);
      Rs := Ts.Create;
	end;
  result := Rss;
end;

function mapExpand(Wss: Tss) : Tss;
  var Hss: Tss; Ws: Ts;
begin
  result := Tss.Create;
  for Ws in Wss do
  begin
    Hss := maybeExpand(Ws);
    if Hss.Count = 0 then
      result.Add(Ws)
     else
      result.AddRange(Hss)
	end;
end;

{ Tss }

destructor Tss.Destroy;
  {TBD var e: Ts;}
begin
  {for e in self do
    e.Free;}
  inherited Destroy;
end;

{ Tsss }

destructor Tsss.Destroy;
  {TBD var e: Tss;}
begin
  {for e in self do
    e.Free;}
  inherited Destroy;
end;

end.

