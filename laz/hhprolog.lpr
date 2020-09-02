(*
 * hhprolog: Hitchhiker Prolog
 *
 * Version: 1.0.0
 * License: MIT
 *
 * Copyright (c) 2018,2019,2020 Carlo Capelli
 *)

program hhprolog;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  ,hhprolog.Toks
  ,hhprolog.Prog
  ,hhprolog.Engine
  ,hhprolog.vector
  ,hhprolog.baseTypes
  ,hhprolog.file2string
  ,hhprolog.clause
  ;

type

  { THHPrologApplication }

  THHPrologApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ THHPrologApplication }

procedure test_run(pl_nl: string);
begin
  with Prog.create(file2string(pl_nl)) do
    begin
      ppCode;
      run(true);
      Free
    end;
end;

{$ifdef test_load}
procedure test_load(pl_nl: string);
begin
  with Prog.create(file2string(pl_nl)) do
    begin
      ppCode;
      Free
    end;
end;
{$endif}

{$ifdef test_tokenize}
procedure test_tokenize(pl_nl: string);
  var
    ltoks: ttoks;
    t: tok;
begin
  ltoks := makeToks(file2string(pl_nl));
  for t in ltoks do
    writeln(format('%s:%s,%d', [t.t,t.s,t.n]));
  ltoks.free
end;
{$endif}

{$ifdef test_baseTypes}
procedure test_baseTypes;
  type
    IVec = specialize hhVector<Integer>;
  var
    i: IVec;
    o: hhObject;
begin
  i := IVec.Create;
  i.PushBack(123);
  i.PushBack(345);
  i.PushBack(567);
  i.Free;

  o := hhObject.create;
  o.Free;
end;
{$endif}

procedure THHPrologApplication.DoRun;
var
  ErrorMsg: String;
  PlNl_file: String;
begin
  // quick check parameters
  ErrorMsg := CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }

  {$if declared(useHeapTrace)}
    setHeapTraceOutput('heaptrc.log');
  {$endIf}

  if GetParamCount >= 1 then
    PlNl_file := GetParams(1)
  else
    PlNl_file := '../test/add.pl.nl';

  {$ifdef test_baseTypes}
  test_baseTypes;
  {$endif}

  {$ifdef test_tokenize}
  test_tokenize(PlNl_file);
  {$endif}

  {$ifdef test_load}
  test_load(PlNl_file);
  {$endif}

  test_run(PlNl_file);

  // stop program loop
  Terminate;
end;

constructor THHPrologApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor THHPrologApplication.Destroy;
begin
  inherited Destroy;
end;

procedure THHPrologApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: THHPrologApplication;
begin
  Application:=THHPrologApplication.Create(nil);
  Application.Title:='HitchHiker Prolog';
  Application.Run;
  Application.Free;
end.

