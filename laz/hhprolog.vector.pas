(*
 * hhprolog: Hitchhiker Prolog
 *
 * Version: 1.0.0
 * License: MIT
 *
 * Copyright (c) 2018,2019,2020 Carlo Capelli
 *)

unit hhprolog.vector;

{$mode objfpc}{$H+}
{$modeswitch arrayoperators+}

interface

uses
  Classes, SysUtils, gvector;

type

  { hhVector }

  generic hhVector<T> = class(specialize TVector<T>)
    public
      constructor Create;
      constructor Create(res: SizeInt);

      function Slice(s: integer): hhVector;
      function Concat(s: hhVector): hhVector;
      function Capacity: Cardinal; inline;
  end;

implementation

constructor hhVector.Create;
begin
  inherited
end;

constructor hhVector.Create(res: SizeInt);
begin
  Create;
  Reserve(res)
end;

function hhVector.Slice(s: integer): hhVector;
begin
  result := hhVector.Create;
  while s < Size do
  begin
    result.PushBack(self[s]);
    Inc(s)
  end
end;

function hhVector.Concat(s: hhVector): hhVector;
  var e: T;
begin
  result := hhVector.Create;
  for e in self do
    result.PushBack(e);
  for e in s do
    result.PushBack(e);
end;

function hhVector.Capacity: Cardinal;
begin
  result := Size { FCapacity is private... }
end;

end.

