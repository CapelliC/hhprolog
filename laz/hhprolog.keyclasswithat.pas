(*
 * hhprolog: Hitchhiker Prolog
 *
 * Version: 1.0.0
 * License: MIT
 *
 * Copyright (c) 2018,2019,2020 Carlo Capelli
 *)

unit hhprolog.KeyClassWithAt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Generics.Collections, Generics.Defaults;

type
  generic TKeyClassWithAt<TKey, TValue{: TObject}> = class(specialize TFastHashMap<TKey, TValue>)
    function At(const AKey: TKey): TValue;
  end;

implementation

function TKeyClassWithAt.At(const AKey: TKey): TValue;
begin
  if not TryGetValue(AKey, Result) then
  begin
    Result := TValue.Create;
    Add(AKey, Result)
  end;
end;

end.

