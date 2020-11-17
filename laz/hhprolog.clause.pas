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

hhClauses = specialize hhVector<Clause>;
hhSyms = specialize hhVector<string>;

implementation

{ Clause }

constructor Clause.Create;
begin
  xs[0] := -1; xs[1] := -1; xs[2] := -1;
end;

destructor Clause.Destroy;
begin
  hgs.Free;
  inherited Destroy;
end;

end.

