(*
 * hhprolog: Hitchhiker Prolog
 *
 * Version: 1.0.0
 * License: MIT
 *
 * Copyright (c) 2018,2019,2020 Carlo Capelli
 *)

unit hhprolog.baseTypes;

{$mode objfpc}{$H+}
{$modeswitch arrayoperators+}

interface

uses
  Classes, SysUtils,
  generics.Collections
  ,hhprolog.vector
  ,hhprolog.recobj
  ,hhprolog.HashMapWithAt
  ,hhprolog.KeyClassWithAt
  ;

type

  Int = Integer;
  size_t = Cardinal;
  cstr = string;

  IntS = specialize hhVector<Int>;
  IntList = IntS;
  IntStack = IntS;

  t_IMaps = specialize TKeyClassWithAt<Int, IntS>;
  t_IntMap = specialize THashMapWithAt<Int, Int>;
  t_VMap = specialize hhVector<t_IntMap>;

  t_refs = specialize TKeyClassWithAt<string, IntS>;

  { hhObject }

  hhObject = TRecObj;
  hhObjects = TVecRecObj;

  Operator +(AString:String;ALongInt:LongInt):String;Overload;
  (*Operator +(ALongInt:LongInt;AString:String):String;Overload;*)

implementation

  Operator +(AString:String;ALongInt:LongInt):String;
  Begin
    Result:=AString+ALongInt.ToString;
  end;
  (*Operator +(ALongInt:LongInt;AString:String):String;
  Begin
    Result:=ALongInt.ToString+AString;
  end;*)

end.

