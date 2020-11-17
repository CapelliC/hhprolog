(*
 * hhprolog: Hitchhiker Prolog
 *
 * Version: 1.0.0
 * License: MIT
 *
 * Copyright (c) 2018,2019,2020 Carlo Capelli
 *)

unit hhprolog.spine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  hhprolog.clause,
  hhprolog.vector,
  hhprolog.baseTypes,
  hhprolog.HashMapWithAt;

type

  (**
   * runtime representation of an immutable list of goals
   * together with top of heap and trail pointers
   * and current clause tried out by head goal
   * as well as registers associated to it
   *
   * note that parts of this immutable lists
   * are shared among alternative branches
   *)

  { Spine }

  Spine = class
      hd: Int;     // head of the clause to which this corresponds
      base: Int;   // top of the heap when this was created

      gs: IntList; // goals - with the top one ready to unfold
      ttop: Int;   // top of the trail when this was created

      k: Int;
      xs: t_xs;    // index elements
      cs: IntS;    // array of clauses known to be unifiable with top goal in gs

      constructor Create;
      constructor Create(_hd: Int; _ttop: Int);

      destructor Destroy; override;
  end;

  hhSpines = specialize hhVector<Spine>;
  tSpineMem = specialize THashMapWithAt<int, int>;

implementation

{ Spine }

constructor Spine.Create;
begin
  xs[0] := -1; xs[1] := -1; xs[2] := -1;
  gs := IntList.create;
  cs := IntS.create;
end;

(*
 * creates a specialized spine returning an answer (with no goals left to solve)
 *)
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

end.

