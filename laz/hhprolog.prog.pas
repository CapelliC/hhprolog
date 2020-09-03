(*
 * hhprolog: Hitchhiker Prolog
 *
 * Version: 1.0.0
 * License: MIT
 *
 * Copyright (c) 2018,2019,2020 Carlo Capelli
 *)

unit hhprolog.Prog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  hhprolog.Engine,
  hhprolog.baseTypes,
  hhprolog.recobj,
  hhprolog.clause;

type

  T_Ask = Term;

  { Prog }

  Prog = class(Engine)
    //constructor Create(s: string);
    procedure run(print_ans: boolean);
    procedure ppCode;

  protected

    function showClause(s: Clause): string;
    function showTerm_o(O: Term): string; override;
    class function st0(const args: Terms): string;
    class function maybeNull(const O: Term): string;
    class function isListCons(name: cstr): boolean; inline;
    class function isOp(name: cstr): boolean; inline;

  end;

implementation
{
constructor Prog.Create(s: string);
begin
  inherited;
end;
}
procedure pp(s: string);
begin
  writeln(s);
end;

function S(i: integer): string;
begin
  exit(IntToStr(i));
end;

function showTerm_o(t: T_Ask): string;
begin
  exit(t.toString);
end;

function Prog.showClause(s: Clause): string;
var
  i, l, e: Int;
  r: string;
begin

  l := s.hgs.size();
  r := '---base:[' + IntToStr(s.base) + '] neck: ' + s.neck + '-----' + LineEnding;
  r := r + showCells2(s.base, s.len); // TODO
  r := r + LineEnding;
  r := r + showCell(s.hgs[0]);

  r := r + ' :- [';
  for i := 1 to l - 1 do
  begin
    e := s.hgs[i];
    r := r + showCell(e);
    if i < l - 1 then
      r := r + ', ';
  end;

  r := r + ']' + LineEnding;

  r := r + showTerm_i(s.hgs[0]);
  if l > 1 then
  begin
    r := r + ' :- ' + LineEnding;
    for i := 1 to l - 1 do
    begin
      e := s.hgs[i];
      r := r + '  ';
      r := r + showTerm_i(e);
      r := r + LineEnding;
    end;
  end
  else
    r := r + LineEnding;
  Result := r
end;

function Prog.showTerm_o(O: Term): string;
begin
  case O.o.tag of
    rt_i: exit(showTerm_i(O.o.i));
    rt_vo: exit(st0(O.o.vo));
  else
    exit(O.toString);
  end;
end;

class function Prog.st0(const args: Terms): string;
  var
    r: string = '';
    name, qname: string;
    tail: Term;
    list: Terms;
    i: Int;
begin
  if args.size > 0 then
  begin
    name := args[0].toString;
    if (args.size = 3) and isOp(name) then
      begin
        r := r + '(';
        r := r + maybeNull(args[0]);
        r := r + ' ' + name + ' ';
        r := r + maybeNull(args[1]);
        r := r + ')';
      end
    else if (args.size = 3) and isListCons(name) then
      begin
        r := r + '[';
        r := r + maybeNull(args[1]);
        tail := args[2];
        while true do
        begin
          if ('[]' = tail.toString) or ('nil' = tail.toString) then
            break;
          if tail.o.tag <> rt_vo then
            begin
              r := r + '|';
              r := r + maybeNull(tail);
              break
            end;
          list := tail.o.vo;
          if not ((list.size = 3) and isListCons(list[0].toString)) then
            begin
              r := r + '|';
              r := r + maybeNull(tail);
              break
            end
          else
            begin
              r := r + ',';
              r := r + maybeNull(list[1]);
              tail := list[2]
            end;
        end;

        r := r + ']';
      end
    else if (args.size() = 2) and ('$VAR' = name) then
      r := r + '_' + args[1].toString
    else
      begin
        qname := maybeNull(args[0]);
        r := r + qname;
        r := r + '(';
        for i := 1 to args.size - 1 do
        begin
          r := r + maybeNull(args[i]);
          if i < args.size - 1 then
              r := r + ','
        end;
        r := r + ')';
      end;
  end;
  exit(r);
end;

class function Prog.maybeNull(const O: Term): string;
begin
  if O.o.tag = rt_ then
    exit('$null');
  if O.o.tag = rt_vo then
    exit(st0(O.o.vo));
  exit(O.toString)
end;

class function Prog.isListCons(name: cstr): boolean;
begin
  result := ('.' = name) or ('[|]' = name) or ('list' = name)
end;

class function Prog.isOp(name: cstr): boolean;
begin
  result := ('/' = name) or ('-' = name) or ('+' = name) or ('=' = name)
end;

procedure Prog.run(print_ans: boolean);
var
  ctr: integer = 0;
  A: T_Ask;
begin
  while True do
  begin
    A := ask;
    if A.o.tag = rt_ then
      begin
        A.free;
        break;
      end;
    if print_ans then
      pp('[' + S(ctr) + '] ' + '*** ANSWER=' + showTerm_o(A));
    A.free;
  end;
  pp('TOTAL ANSWERS=' + S(ctr));
end;

procedure Prog.ppCode;
  var
    t: string = '';
    i: size_t;
    C: Clause;
begin
  for i := 0 to syms.size - 1 do
  begin
    if i > 0 then
      t := t + ', ';
    t := t + syms[i] + '=' + i
  end;

  pp(LineEnding+'SYMS:'+LineEnding+'{' + t + '}');

  pp(LineEnding+'CLAUSES:'+LineEnding);
  for i := 0 to clauses.size - 1 do
  begin
    C := clauses[i];
    pp('[' + i + ']:' + showClause(C));
  end;

  pp('');
end;

end.
