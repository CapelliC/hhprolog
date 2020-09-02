unit fatalerr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure fatal(msg: string; args: array of const);
function validate(check: boolean; iftrue: string; msg: string; args: array of const): string;

implementation

procedure fatal(msg: string; args: array of const);
begin
  raise Exception.Create(Format(msg, args))
end;

function validate(check: boolean; iftrue: string; msg: string; args: array of const): string;
begin
  if (not check) then
    fatal(msg, args);
  validate := iftrue
end;

end.

