(*
 * hhprolog: Hitchhiker Prolog
 *
 * Version: 1.0.0
 * License: MIT
 *
 * Copyright (c) 2018,2019,2020 Carlo Capelli
 *)

unit hhprolog.clause;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  hhprolog.vector,
  hhprolog.baseTypes,
  hhprolog.HashMapWithAt;

const MINSIZE = 1 << 15;
const MAXIND = 3;
const START_INDEX = 20;

type

t_xs = array[0..MAXIND-1] of integer;

{ Clause }

Clause = class
  var
    len: Int;
    hgs: IntS;
    base: Int;
    neck: Int;
    xs: t_xs;

    constructor Create;
    destructor Destroy; override;
end;

{ Spine }

Spine = class
    hd: Int;     // head of the clause to which this corresponds
    base: Int;   // top of the heap when this was created

    gs: IntList; // goals - with the top one ready to unfold
    ttop: Int;   // top of the trail when this was created

    k: Int;
    xs: t_xs;    // index elements
    cs: IntS;    // array of  clauses known to be unifiable with top goal in gs

    constructor Create;
    constructor Create(_hd: Int; _ttop: Int);

    destructor Destroy; override;
end;

hhClauses = specialize hhVector<Clause>;
hhSpines = specialize hhVector<Spine>;
hhSyms = specialize hhVector<string>;

tSpineMem = specialize THashMapWithAt<int, int>;

implementation

{ Spine }

constructor Spine.Create;
begin
  xs[0] := -1; xs[1] := -1; xs[2] := -1;
  gs := IntList.create;
  cs := IntS.create;
end;

constructor Spine.Create(_hd: Int; _ttop: Int);
begin
  Create;
  hd := _hd;
  base := 0;
  ttop := _ttop;
  k := -1;
end;

destructor Spine.Destroy;
begin
  inherited Destroy;
  gs.Free;
  cs.Free;
end;

{ Clause }

constructor Clause.Create;
begin
  xs[0] := -1; xs[1] := -1; xs[2] := -1;
  hgs := IntS.create;
end;

destructor Clause.Destroy;
begin
  hgs.Free;
  inherited Destroy;
end;

end.

