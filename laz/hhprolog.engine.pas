(*
 * hhprolog: Hitchhiker Prolog
 *
 * Version: 1.0.0
 * License: MIT
 *
 * Copyright (c) 2018,2019,2020 Carlo Capelli
 *)

unit hhprolog.Engine;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, math
    ,hhprolog.baseTypes
    ,hhprolog.clause
    ,hhprolog.recobj
    ,hhprolog.toks
    ,fatalerr
    ;

{$macro on}
{$define INL:=}

const
	V = 0;
	U = 1;
	R = 2;
	C = 3;
	N = 4;
	A = 5;
	BAD = 7;

  K_POOL = 10000;
  K_PUSHBODY = 100;

type

  { Engine }

  Engine = class
    constructor Create(asm_nl_source: string);
    destructor Destroy; override;

    function stats: string;

  protected

    syms: hhSyms;
    clauses: hhClauses;
    cls: IntS;
    heap: IntS;
    top: Int; //  = -1;
    trail: IntStack;
    ustack: IntStack;

    spines: hhSpines;
    spines_top: cardinal;

    query: Spine; // = nullptr;

    imaps: t_imaps;
    vmaps: t_vmap;

    gs_pushBody: IntS;
    c_inferences: size_t;

    // raw display of a term - to be overridden
    function showTerm_i(x: Int): string; virtual;

    // raw display of a externalized term
    function showTerm_o(O: Term): string; virtual;

    function showCell(w: Int): string;
    function showCells2(base: Int; len: Int): string;
    function showCells1(cs: IntS): string;

    function new_spine(gs0: IntS; base: Int; rgs: IntList; ttop: Int): Spine;
    class function tag(t, w: Int): Int; inline;
    class function detag(w: Int): Int; inline;
    class function tagOf(w: Int): Int; inline;
    class function isVAR(x: Int): boolean; inline;

    class procedure pp(s: string);

    class function tagSym(t: Int): cstr;
    class function heapCell(w: Int): cstr;

    class function toNums(_clauses: hhClauses): IntS;

    class function getSpine(const cs: IntS): IntS;
    class function relocate(b: Int; cell: Int): Int; INL//inline;

    class function hasClauses(const S: Spine): boolean;
    class function hasGoals(const S: Spine): boolean;
    function addSym(sym: cstr): Int;
    function getSym(w:Int): cstr;

    procedure makeHeap(size:Int = MINSIZE);

    procedure clear;
    procedure push(i: Int); INL//inline;

    const function size: Int; INL//inline;

    procedure expand;
    procedure ensureSize(more: Int);

    function dload(s: cstr): hhClauses;

    function getRef(x: Int): Int; INL//inline;
    procedure setRef(w, r: Int); INL//inline;
    function encode(t: Int; s: cstr): Int; INL//inline;

    procedure unwindTrail(savedTop: Int);
    function deref(x: Int): Int;

    procedure ppTrail;
    function exportTerm(x: Int): Term;

    {$ifdef ppengine}
    procedure ppc(const c: Clause);
    procedure ppGoals(const gs: IntS);
    procedure ppSpines;
    {$endif}

    function unify(base: Int): boolean;
    function unify_args(w1, w2: Int): boolean;

    function putClause(cs, gs: IntS; neck: Int): Clause;

    procedure pushCells1(b, from, &to, base: Int);
    procedure pushCells2(b, from, &to: Int; const cs: IntS);
    function pushHead(b: Int; const C: Clause): Int;

    procedure pushBody(b, head: Int; C: Clause);

    procedure makeIndexArgs(G: Spine);
    procedure getIndexables(ref: Int; c: Clause);
    function cell2index(cell: Int): Int;

    class function match(xs: t_xs; const C0: Clause): boolean; INL//inline;

    function unfold: Spine;

    function getQuery: Clause; INL//inline;
    function init: Spine;
    function answer(ttop: Int): Spine;
    procedure popSpine;

    function yield_: Spine;
    function ask: Term;

    function vcreate(l: size_t): hhprolog.Toks.Tss; INL//inline;

    procedure put(keys: t_xs; val: Int); INL//inline;
    procedure index(_clauses: hhClauses);

private
    c_spine_mem: tSpineMem;

	end;

implementation

constructor Engine.Create(asm_nl_source: string);
begin
  top := -1;
  syms := hhSyms.create;
  makeHeap;
  clauses := dload(asm_nl_source);
  cls := toNums(clauses);
  query := init
end;

destructor Engine.Destroy;
begin
  syms.free;
  heap.free;
  trail.free;
  ustack.free;
  clauses.free;
  spines.free;
  cls.free;
  c_spine_mem.free;
  gs_pushBody.free;
end;

function Engine.stats: string;
  var c: int;
begin
  result :=
    heap.size.toString + ' '
      + spines_top + ' of ' + spines.size + ' '
      + trail.size + ' '
      + ustack.size + ' [ ';
  for c in c_spine_mem.keys do
    result := result + c + ':' + c_spine_mem[c] + ' ';
  result := result + ']';
end;

function Engine.showTerm_i(x: Int): string;
  var t: Term;
begin
  t := exportTerm(x);
  result := showTerm_o(t);
  t.free
end;

function Engine.showTerm_o(O: Term): string;
begin
    result := O.toString;
end;

function Engine.showCell(w: Int): string;
  var
    t, val: Int;
begin
  t := tagOf(w);
  val := detag(w);
  case t of
  V:   result := 'v:'     + val.toString;
  U:   result := 'u:'     + val.toString;
  N:   result := 'n:'     + val.toString;
  C:   result := 'c:'     + getSym(val);
  R:   result := 'r:'     + val.toString;
  A:   result := 'a:'     + val.toString;
  else result := '*BAD*=' + w.toString
  end;
end;

function Engine.showCells2(base: Int; len: Int): string;
var
  buf: string = '';
  k, instr: Int;
begin
  for k := 0 to len - 1 do
  begin
    instr := heap[base + k];
    buf := buf + '[' + (base + k) + ']' + showCell(instr) + ' ';
  end;
  exit(buf);
end;

function Engine.showCells1(cs: IntS): string;
var
  k: Int;
begin
  result := '';
  for k := 0 to cs.size do
    result := result + '[' + k + ']' + showCell(cs[k]) + ' ';
end;

function Engine.new_spine(gs0: IntS; base: Int; rgs: IntList; ttop: Int): Spine;
  var
    sp: Spine;
    req_size, x, y: size_t;
begin
  if not assigned(spines[spines_top]) then
    spines[spines_top] := Spine.create;

  sp := spines[spines_top];
  inc(spines_top);

  sp.hd := gs0[0];
  sp.cs := cls;
  sp.base := base;
  sp.ttop := ttop;
  sp.xs[0] := -1; sp.xs[1] := -1; sp.xs[2] := -1;
  sp.k := 0;
  // note: cannot reuse G because the last spines.push_back could relocate the array
  //auto req_size = gs0.size() - 1 + ( rgs.size() > 0 ? rgs.size() -1 : 0 );
  req_size := gs0.size - 1;
  if rgs.size > 0 then
    req_size := req_size + rgs.size - 1;
{$IF 0}
  sp->gs.reserve(req_size);

  for (size_t x = 1; x < gs.size(); ++x)
      sp->gs.push_back(gs[x]);
  for (size_t x = 1; x < rgs.size(); ++x)
      sp->gs.push_back(rgs[x]);
{$ELSE}
  sp.gs.resize(req_size);
  y := 0;
  if gs0.size > 0 then
    for x := 1 to gs0.size - 1 do
    begin
      sp.gs[y] := gs0[x];
      inc(y)
    end;
  if rgs.size > 0 then
    for x := 1 to rgs.size - 1 do
    begin
      sp.gs[y] := rgs[x];
      inc(y)
    end;
{$ENDIF}
  //c_spine_mem[req_size]++;
  result := sp;
end;

class function Engine.tag(t, w: Int): Int;
begin
  result := -((w << 3) + t)
end;

class function Engine.detag(w: Int): Int;
begin
  result := -w >> 3
end;

class function Engine.tagOf(w: Int): Int;
begin
  result := -w and 7
end;

class function Engine.isVAR(x: Int): boolean;
begin
  result := tagOf(x) < 2
end;

class procedure Engine.pp(s: string);
begin
  Writeln(s)
end;

class function Engine.tagSym(t: Int): cstr;
begin
  if t = V then exit('V');
  if t = U then exit('U');
  if t = R then exit('R');
  if t = C then exit('C');
  if t = N then exit('N');
  if t = A then exit('A');
  exit('?')
end;

class function Engine.heapCell(w: Int): cstr;
begin
  exit(tagSym(tagOf(w)) + ':' + detag(w) + '[' + w + ']');
end;

class function Engine.toNums(_clauses: hhClauses): IntS;
  var i: Int;
begin
  result := IntS.create;
  for i := 0 to _clauses.size - 1 do
    result.PushBack(i);
end;

class function Engine.getSpine(const cs: IntS): IntS;
  var x, t, a, w, i: Int;
begin
  result := IntS.create;
  a := cs[1];
  w := detag(a);
  for i := 0 to w - 1 - 1 do
  begin
    x := cs[3 + size_t(i)];
    t := tagOf(x);
    if R <> t then
        raise exception.create('*** getSpine: unexpected tag=' + t.ToString());
    result.PushBack(detag(x))
  end
end;

class function Engine.relocate(b: Int; cell: Int): Int;
begin
  if tagOf(cell) < 3 then
    result := cell + b
  else
    result := cell
end;

class function Engine.hasClauses(const S: Spine): boolean;
begin
  result := S.k < S.cs.size
end;

class function Engine.hasGoals(const S: Spine): boolean;
begin
  result := S.gs.size > 0
end;

function Engine.addSym(sym: cstr): Int;
  var i: Int;
begin
  if syms.size > 0 then
    for i := 0 to syms.size - 1 do
      if syms[i] = sym then
        exit(i);
  syms.PushBack(sym);
  exit(syms.size - 1);
end;

function Engine.getSym(w: Int): cstr;
begin
  if (w < 0) or (w >= syms.size) then
    raise Exception.create('BADSYMREF=' + IntToStr(w));
  result := syms[w]
end;

procedure Engine.makeHeap(size: Int);
begin
  heap := IntS.create;
  heap.resize(size);
  clear
end;

procedure Engine.clear;
begin
  top := -1
end;

procedure Engine.push(i: Int);
begin
  inc(top);
  heap[top] := i
end;

function Engine.size: Int;
begin
  result := top + 1
end;

procedure Engine.expand;
begin
  heap.resize(heap.size * 2)
end;

procedure Engine.ensureSize(more: Int);
begin
  if 1 + top + more >= heap.size then
    expand
end;

function Engine.dload(s: cstr): hhClauses;
  var
    Wsss: Tsss;
    Wss, Rss: Tss;
    Ws: Ts;
    cs, gs{, tgs}, &Is{, r_}: IntS;
    k, l, leader, i, j, neck: Int;
    L_, kIs, w, w_: string;
    refs: t_refs;
begin
  result := hhClauses.create;
  Wsss := toSentences(s);
  for Wss in Wsss do
  begin
    refs := t_refs.create;
    cs := IntS.create;
    gs := IntS.create;
    Rss := mapExpand(Wss);
    k := 0;
    for ws in Rss do
    begin
      l := ws.Count;
      gs.PushBack(tag(R, k)); inc(k);
      cs.PushBack(tag(A, l));
      for w_ in ws do
      begin
        w := w_;
        if 1 = length(w) then
          w := 'c:' + w;
        L_ := w.Substring(2);
        case w.Chars[0] of
        'c': begin
          cs.PushBack(encode(C, L_));
          inc(k);
          end;
        'n': begin
          cs.PushBack(encode(N, L_));
          inc(k);
          end;
        'v': begin
          //r_ := refs.At(L_);
          //r_.PushBack(k);
          refs.At(L_).PushBack(k);
          cs.PushBack(tag(BAD, k));
          inc(k);
          end;
        'h': begin
          refs.At(L_).PushBack(k - 1);
          cs[k - 1] := tag(A, l - 1);
          gs.PopBack();
          end;
        else
          raise exception.create('FORGOTTEN=' + w)
        end
      end;
    end;

    for kIs in refs.keys do
    begin
      &Is := refs[kIs];
      leader := -1;
      for j in &Is do
        if A = tagOf(cs[j]) then
        begin
          leader := j;
          break;
        end;

      if -1 = leader then
      begin
        leader := &Is[0];
        for i in &Is do
          if i = leader then
            cs[i] := tag(V, i)
          else
            cs[i] := tag(U, leader)
      end
      else
        for i in &Is do
        begin
          if i = leader then
            continue;
          cs[i] := tag(R, leader)
        end;
    end;
    if 1 = gs.size then
      neck := cs.size
    else
      neck := detag(gs[1]);
    result.PushBack(putClause(cs, gs, neck));

    refs.free;
    cs.free;
    Rss.Free;
  end;
  Wsss.Free
end;

function Engine.getRef(x: Int): Int;
begin
  result := heap[detag(x)]
end;

procedure Engine.setRef(w, r: Int);
begin
  heap[detag(w)] := r
end;

function Engine.encode(t: Int; s: cstr): Int;
  var w: Int;
begin
  if C = t then
    w := addSym(s)
  else
    w := StrToInt(s);
  result := tag(t, w)
end;

procedure Engine.unwindTrail(savedTop: Int);
  var href: Int;
begin
  while savedTop < integer(trail.size) - 1 do
  begin
      href := trail[trail.size - 1];
      trail.PopBack;
      setRef(href, href);
  end;
end;

function Engine.deref(x: Int): Int;
  var r: Int;
begin
  while isVAR(x) do begin
      r := getRef(x);
      if r = x then
          break;
      x := r;
  end;
  result := x;
end;

procedure Engine.ppTrail;
  var i, t: Int;
begin
  for i := 0 to trail[trail.size - 1] do
  begin
    t := trail[i];
    pp('trail[' + IntToStr(i) + ']=' + showCell(t) + ':' + showTerm_i(t));
  end
end;

function Engine.exportTerm(x: Int): Term;
  var
    t, w, a_, n_, k, i, j: Int;
    args: TVecRecObj;
begin
  x := deref(x);
  t := tagOf(x);
  w := detag(x);

  case t of
    C:  result := Term.create(getSym(w));
    N:  result := Term.create(w);
    V:  result := Term.create('V' + w.ToString);
    R: begin
      a_ := heap[w];
      if A <> tagOf(a_) then
        raise exception.create('*** should be A, found=' + showCell(a_));
      n_ := detag(a_);
      args := TVecRecObj.create;
      k := w + 1;
      for i := 0 to n_ - 1 do
      begin
        j := k + i;
        args.PushBack(exportTerm(heap[j]))
      end;
      result := Term.create(args);
      args.Free
    end;
    else
      fatal('*BAD TERM* %s', [showCell(x)]);
  end
end;

{$ifdef ppengine}
procedure Engine.ppc(const c: Clause);
begin
end;

procedure Engine.ppGoals(const gs: IntS);
begin
end;

procedure Engine.ppSpines;
begin
end;
{$endif}

function Engine.unify(base: Int): boolean;
  var
    x1, x2, t1, t2, w1, w2: Int;
begin
  while ustack.size > 0 do
  begin
    x1 := deref(ustack.back); ustack.popback;
    x2 := deref(ustack.back); ustack.popback;
    if x1 <> x2 then
      begin
        t1 := tagOf(x1);
        t2 := tagOf(x2);
        w1 := detag(x1);
        w2 := detag(x2);
        if isVAR(x1) then
        begin
          if isVAR(x2) and (w2 > w1) then
          begin
            heap[w2] := x1;
            if w2 <= base then
              trail.PushBack(x2)
          end
          else
          begin
            heap[w1] := x2;
            if w1 <= base then
              trail.PushBack(x1)
          end
        end
        else if isVAR(x2) then
        begin
          heap[w2] := x1;
          if w2 <= base then
            trail.PushBack(x2)
        end
        else if (R = t1) and (R = t2) then
        begin
          if not unify_args(w1, w2) then
            exit(false)
        end
      end
      else
        exit(false)
  end;
  exit(true);
end;

function Engine.unify_args(w1, w2: Int): boolean;
  var
    v1, v2, n1, n2, b1, b2, i1, i2, u1, u2, i: Int;
begin
  v1 := heap[size_t(w1)];
  v2 := heap[size_t(w2)];
  // both should be A
  n1 := detag(v1);
  n2 := detag(v2);
  if n1 <> n2 then
    exit(false);
  b1 := 1 + w1;
  b2 := 1 + w2;
  for i := n1 - 1 downto 0 do
  begin
    i1 := b1 + i;
    i2 := b2 + i;
    u1 := heap[i1];
    u2 := heap[i2];
    if u1 = u2 then
        continue;
    ustack.PushBack(u2);
    ustack.PushBack(u1)
  end;
  exit(true)
end;

function Engine.putClause(cs, gs: IntS; neck: Int): Clause;
  var
    base, b, len, i: Int;
    XC: Clause;
begin
  base := size;
  b := tag(V, base);
  len := cs.size;
  pushCells2(b, 0, len, cs);
  for i := 0 to gs.size - 1 do
      gs[i] := relocate(b, gs[i]);
  XC := Clause.create;
  getIndexables(gs[0], XC);
  XC.len := len;
  XC.hgs := gs;
  XC.base := base;
  XC.neck := neck;
  exit(XC)
end;

procedure Engine.pushCells1(b, from, &to, base: Int);
  var i: Int;
begin
  ensureSize(&to - from);
  for i := from to &to - 1 do
    push(relocate(b, heap[base + i]));
end;

procedure Engine.pushCells2(b, from, &to: Int; const cs: IntS);
  var i: Int;
begin
  ensureSize(&to - from);
  for i := from to &to - 1 do
    push(relocate(b, cs[i]));
end;

function Engine.pushHead(b: Int; const C: Clause): Int;
begin
  pushCells1(b, 0, C.neck, C.base);
  result := relocate(b, C.hgs[0]);
end;

procedure Engine.pushBody(b, head: Int; C: Clause);
  var
    l, k: size_t;
    cell: Int;
begin
  pushCells1(b, C.neck, C.len, C.base);
  l := C.hgs.size;
  gs_pushBody.resize(l);
  gs_pushBody[0] := head;
  for k := 1 to l - 1 do
  begin
    cell := C.hgs[k];
    gs_pushBody[k] := relocate(b, cell);
  end;
end;

procedure Engine.makeIndexArgs(G: Spine);
  var
    goal, p, n, i, cell: Int;
begin
  if G.xs[0] <> -1 then
      exit;

  goal := G.gs[0];
  p := 1 + detag(goal);
  n := min(MAXIND, detag(getRef(goal)));
  for i := 0 to n - 1 do
  begin
    cell := deref(heap[p + i]);
    G.xs[i] := cell2index(cell);
  end;
end;

procedure Engine.getIndexables(ref: Int; c: Clause);
  var
    p, n, i, cell: Int;
begin
  p := 1 + detag(ref);
  n := detag(getRef(ref));
  i := 0;
  while (i < MAXIND) and (i < n) do
  begin
    cell := deref(heap[p + i]);
    c.xs[i] := cell2index(cell);
    inc(i)
  end
end;

function Engine.cell2index(cell: Int): Int;
  var x, t: Int;
begin
  x := 0;
  t := tagOf(cell);
  case t of
  R:   x := getRef(cell);
  C,N: x := cell;
  end;
  exit(x)
end;

class function Engine.match(xs: t_xs; const C0: Clause): boolean;
  var i, x, y: Int;
begin
  for i := 0 to MAXIND-1 do
  begin
    x := xs[i];
    y := C0.xs[i];
    if (0 = x) or (0 = y) then
        continue;
    if x <> y then
        exit(false);
  end;
  result := true;
end;

function Engine.unfold: Spine;
  var
    G: Spine;
    C0: Clause;
    ttop, htop, base: Int;
    last, k: size_t;
    base0, b, head: Int;
begin
  inc(c_inferences);

  G := spines[spines_top - 1];

  ttop := integer(trail.size) - 1;
  htop := top;
  base := htop + 1;

  //cout << c_inferences << ' ' << ttop << ' ' << htop << ' ' << base << endl;

  makeIndexArgs(G);

  last := G.cs.size;
  for k := G.k to last - 1 do
  begin
    C0 := clauses[G.cs[k]];
    if not match(G.xs, C0) then
        continue;
    base0 := base - C0.base;
    b := tag(V, base0);
    head := pushHead(b, C0);
    ustack.clear;
    ustack.PushBack(head);
    ustack.PushBack(G.gs[0]);
    if not unify(base) then
    begin
      unwindTrail(ttop);
      top := htop;
      continue;
    end;

    pushBody(b, head, C0);
    G.k := k + 1;
    if (gs_pushBody.size > 1) or (G.gs.size > 1) then
      exit(new_spine(gs_pushBody, base, G.gs, ttop))
    else
      exit(answer(ttop))
  end;
  exit(nil)
end;

function Engine.getQuery: Clause;
begin
  result := clauses.back
end;

function Engine.init: Spine;
  var base: Int; G: Clause;
begin
  base := size;
  G := getQuery;

  trail := IntStack.create(K_POOL);
  ustack := IntStack.create(K_POOL);

  spines := hhSpines.create(K_POOL);
  spines.resize(K_POOL);
  spines_top := 0;

  gs_pushBody := IntS.create(K_PUSHBODY);

  result := new_spine(G.hgs, base, IntS.create, -1)
end;

function Engine.answer(ttop: Int): Spine;
begin
  exit(Spine.create(spines[0].hd, ttop))
end;

procedure Engine.popSpine;
begin
  dec(spines_top);
  unwindTrail(spines[spines_top].ttop);
  top := spines[spines_top].base - 1
end;

function Engine.yield_: Spine;
  var
    C: Spine;
begin
  while spines_top > 0 do
  begin
    C := unfold;
    if nil = C then
    begin
      popSpine; // no matches
      continue
    end;
    if hasGoals(C) then
      continue;
    exit(C) // answer
  end;
  exit(nil)
end;

function Engine.ask: Term;
  var
    res: Int;
    ans: Spine;
begin
  query := yield_();
  if nil = query then
      exit(Term.Create);
  ans := answer(query.ttop);
  res := ans.hd;
  result := exportTerm(res);
  unwindTrail(query.ttop);
  ans.Free;
  FreeAndNil(query)
end;

function Engine.vcreate(l: size_t): hhprolog.Toks.Tss;
begin
  result := hhprolog.Toks.Tss.create;
  while l > 0 do
    begin
      result.Add(Ts.create);
      dec(l)
    end;
end;

procedure Engine.put(keys: t_xs; val: Int);
  var
    i, key: Int;
    t: IntS;
begin
  for i := 0 to imaps.Count - 1 do
  begin
    key := keys[i];
    if key <> 0 then begin
      t := imaps[i];
      t[key] := val
    end
    else begin
      vmaps[i][val] := val
    end

  end
end;

procedure Engine.index(_clauses: hhClauses);
var i: size_t; c: Clause;
begin
  if _clauses.size < START_INDEX then
      exit;
  imaps := t_imaps.create(vmaps.size);
  for i := 0 to _clauses.size - 1 do
  begin
    c := _clauses[i];
    //pp("!!!xs=" + T(c.xs) + ":" + this.showCells1(c.xs) + "=>" + i)
    put(c.xs, Int(i + 1)); // $$$ UGLY INC
    //pp(T(imaps));
  end;

  (*
  pp("INDEX");
  pp(T(imaps));
  pp(T(vmaps));
  pp("");
  *)

end;

end.


