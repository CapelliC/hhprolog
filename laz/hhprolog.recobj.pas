(*
 * hhprolog: Hitchhiker Prolog
 *
 * Version: 1.0.0
 * License: MIT
 *
 * Copyright (c) 2018,2019,2020 Carlo Capelli
 *)

unit hhprolog.recobj;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gvector;

type

  TRecObj = class;

  { TVecRecObj }

  TVecRecObj = class(specialize TVector<TRecObj>)
    constructor Create;
    constructor Create(args: array of const);
    destructor Destroy; override;

    function Clone: TVecRecObj;
  end;

  { TRecObj }

  TRecObj = class
    type r_tag = (rt_, rt_i, rt_ps, rt_vo);
  var
    o: record
      case tag: r_tag of
        rt_i: (i: integer);
        rt_ps: (ps: PChar);
        rt_vo: (vo: TVecRecObj);
    end;

    constructor Create;
    constructor Create(i: integer);
    constructor Create(s: string);
    constructor Create(vo: TVecRecObj);
    constructor Create(args: array of const);

    destructor Destroy; override;

    function Clone: TRecObj;
    function toString: string; override;
  end;

implementation

{ TVecRecObj }

function TVecRecObj.Clone: TVecRecObj;
var
  o: TRecObj;
begin
  Result := TVecRecObj.Create;
  Result.Reserve(Size);
  for o in self do
  begin
    Result.PushBack(o.Clone);
  end;
end;

constructor TVecRecObj.Create;
begin
  inherited;
end;

constructor TVecRecObj.Create(args: array of const);
var
  i: integer;
begin
  for i := 0 to High(args) do
    case args[i].VType of
      vtInteger: PushBack(TRecObj.Create(args[i].VInteger));
      vtInt64: PushBack(TRecObj.Create(args[i].VInt64^));
      vtChar: PushBack(TRecObj.Create(args[i].VChar));
      vtString: PushBack(TRecObj.Create(args[i].VString^));
      vtPChar: PushBack(TRecObj.Create(args[i].VPChar));
      else
        raise Exception.Create(Format('arg %d: invalid type %d', [i, args[i].VType]))
    end;
end;

destructor TVecRecObj.Destroy;
var
  o: TRecObj;
begin
  for o in self do
    o.Free;
  inherited Destroy;
end;

{ TRecObj }

constructor TRecObj.Create;
begin
  o.tag := rt_;
end;

constructor TRecObj.Create(i: integer);
begin
  o.tag := rt_i;
  o.i := i;
end;

constructor TRecObj.Create(s: string);
begin
  o.tag := rt_ps;
  o.ps := stralloc(Length(s) + 1);
  strpcopy(o.ps, s);
end;

constructor TRecObj.Create(vo: TVecRecObj);
begin
  o.tag := rt_vo;
  o.vo := vo.Clone;
end;

constructor TRecObj.Create(args: array of const);
begin
  o.tag := rt_vo;
  o.vo := TVecRecObj.Create(args);
end;

destructor TRecObj.Destroy;
begin
  inherited Destroy;
  case o.tag of
    rt_: ;
    rt_i: ;
    rt_ps: strdispose(o.ps);
    rt_vo: o.vo.Free;
  end;
end;

function TRecObj.Clone: TRecObj;
begin
  Result := TRecObj.Create;
  with Result.o do
  begin
    tag := o.tag;
    case tag of
      rt_: ;
      rt_i: i := o.i;
      rt_ps: ps := strnew(o.ps);
      rt_vo: vo := o.vo.Clone;
    end;
  end;
end;

function TRecObj.toString: string;
  var
    j: string = '';
    i: TRecObj;
  begin
    case o.tag of
    	rt_:    exit('$null');
    	rt_i:   exit(IntToStr(o.i));
    	rt_ps:  exit(o.ps);
    	rt_vo:
    		begin
    		  for i in o.vo do
    		  begin
    		    if j <> '' then
    		      j := j + ',';
    		    j := j + i.toString;
    		  end;
    		  exit('(' + j + ')');
    	  end;
      else
    	  raise exception.create('invalid hhObject');
    end;
  end;

end.
