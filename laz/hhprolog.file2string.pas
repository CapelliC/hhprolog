(*
 * hhprolog: Hitchhiker Prolog
 *
 * Version: 1.0.0
 * License: MIT
 *
 * Copyright (c) 2018,2019,2020 Carlo Capelli
 *)

unit hhprolog.file2string;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fatalerr;

function file2string(filepath: string): string;

implementation

function file2string(filepath: string): string;
begin
  if not FileExists(filepath) then
    fatal('cannot find file %s', [filepath]);
  with TStringList.Create do begin
    LoadFromFile(filepath);
    Result := validate(Count > 0, Text, 'file %s is empty', [filepath]);
    Free
  end
end;

end.

