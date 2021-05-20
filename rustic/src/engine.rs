use crate::base_types::{
  Clause, Spine,
  Cardinal, Int, IntList, IntS, IntStack, Refs, TRefs, //TXs,
  MAXIND, MINSIZE,
};

use crate::plscan;
use crate::plscan::{map_expand, sub_str};
use crate::symstore::SymStore;

use std::fs::read_to_string;

use crate::recobj::{RecObj, RecObjs};

type Term = RecObj;

const V: isize = 0;
const U: isize = 1;
const R: isize = 2;
const C: isize = 3;
const N: isize = 4;
const A: isize = 5;
//const B: isize = 6; // CC: add builtins
const BAD: isize = 7;

const K_POOL: isize = 500;
const K_PUSHBODY: isize = 100;

fn tag(t: Int, w: Int) -> Int {
  -((w << 3) + t)
}
fn detag(w: Int) -> Int {
  -w >> 3
}
fn tag_of(w: Int) -> Int {
  -w & 7
}
fn is_var(x: Int) -> bool {
  tag_of(x) < 2
}
/*
fn tag_sym(t: Int) -> char {
  match t {
    V => 'V',
    U => 'U',
    R => 'R',
    C => 'C',
    N => 'N',
    A => 'A',
    _ => '?',
  }
}

fn heap_cell(w: Int) -> String {
  format!("{}:{}[{}]", tag_sym(tag_of(w)), detag(w), w)
}
*/
pub fn to_nums(clauses: &Clauses) -> IntS {
  let mut result = IntS::new();
  for i in 0 .. clauses.len() {
    result.push(i as isize)
  }
  result
}
/*
fn get_spine(cs: IntS) -> IntS {
  let mut result = IntS::new();
  let a = cs[1];
  let w = detag(a);
  for i in 0..w - 1 {
    let x = cs[(3 + i) as usize];
    let t = tag_of(x);
    if R != t {
      panic!("*** getSpine: unexpected tag")
    }
    result.push(detag(x))
  }
  result
}
*/
fn relocate(b: Int, cell: Int) -> Int {
  if tag_of(cell) < 3 {
    cell + b
  } else {
    cell
  }
}

/**
* tests if the head of a clause, not yet copied to the heap
* for execution could possibly match the current goal, an
* abstraction of which has been place in regs
fn match_(xs: &TXs, c0: &Clause) -> bool {
  for i in 0 .. MAXIND {
    let x = xs[i];
    let y = c0.xs[i];
    if (0 == x) || (0 == y) {
      continue;
    }
    if x != y {
      return false;
    }
  }
  true
}
*/

type Clauses = Vec<Clause>;
type Spines = Vec<Spine>;

#[derive(Debug)]
pub struct Engine {

  pub syms: Vec<String>,  // really, should be protected
  pub clauses: Clauses, // = Vec<Clause>::new()

  cls: IntS,
  pub heap: IntS,
  top: Int, //  = -1;
  trail: IntStack,
  ustack: IntStack,

  spines: Spines,
  spines_top: Cardinal,

  query: Option<Spine>, // = nullptr;

  //imaps: t_imaps;
  //vmaps: t_vmap;
  gs_push_body: IntS,
  //c_inferences: size_t;
}
impl Engine {
  pub fn new(asm_nl_source: &str) -> Self {
    let mut e = Engine {
      syms: Vec::<String>::new(),
      clauses: Clauses::new(),
      cls: IntS::new(),
      heap: IntS::new(),
      top: -1,
      trail: IntStack::new(),
      ustack: IntStack::new(),
      spines: Spines::new(),
      spines_top: 0,

      query: None,
      gs_push_body: IntS::new(),
    };
    e.make_heap(MINSIZE as isize);
    e.dload(asm_nl_source);
    e.cls = to_nums(&e.clauses);
    e.query = Some(e.init());
    e
  }

  /**
  * loads a program from a .nl file of
  * "natural language" equivalents of Prolog/HiLog statements
  */
  fn dload(&mut self, asm: &str) {
    let s = read_to_string(asm).unwrap();

    let wsss = plscan::to_sentences(&s);
//    dbg!(wsss.clone());

    for wss in wsss.iter() {
      let mut refs = TRefs::new();
      let mut cs = IntS::new();
      let mut gs = IntS::new();
      let rss = map_expand(wss);

      let mut k = 0;
      for ws in rss {
        let l = ws.len() as Int;
        gs.push(tag(R, k));
        k += 1;
        cs.push(tag(A, l));
        for w_ in ws {
          let mut w = w_;
          if 1 == w.len() {
            w = format!("c:{}", w)
          }
          let ll = sub_str(&w, 2, None);
          match w.chars().take(1).next() {
            Some('c') => {
              cs.push(self.encode(C, ll));
              k += 1;
            }
            Some('n') => {
              cs.push(self.encode(N, ll));
              k += 1;
            }
            Some('v') => {
              refs.at(ll).push(k as usize);
              cs.push(tag(BAD, k));
              k += 1;
            }
            Some('h') => {
              refs.at(ll).push((k - 1) as usize);
              cs[(k - 1) as usize] = tag(A, l - 1);
              gs.pop();
            }
            _ => {
              panic!("FORGOTTEN={}", w)
            }
          }
        }
      }

      for (_, xis) in &refs {
        let mut leader: Int = -1;
        for j in xis {
          if A == tag_of(cs[*j]) {
            leader = *j as Int;
            break;
          }
        }
        if -1 == leader {
          leader = xis[0] as Int;
          for i in xis {
            if *i == leader as usize {
              cs[*i] = tag(V, *i as Int)
            } else {
              cs[*i] = tag(U, leader)
            }
          }
        } else {
          for i in xis {
            if *i as Int == leader {
              continue;
            }
            cs[*i] = tag(R, leader)
          }
        }
      }

      let neck = if 1 == gs.len() {
        cs.len() as Int
      } else {
        detag(gs[1])
      };
      let mut tgs = gs.clone();
      let clause = self.put_clause(cs, &mut tgs, neck);
      self.clauses.push(clause);
    }
  }

  /**
  * encodes string constants into symbols while leaving
  * other data types untouched
  */
  fn encode(&mut self, t: Int, s: String) -> Int {
    let w = if C == t {
      self.add_sym(s)
    } else {
      s.parse::<Int>().unwrap()
    };
    tag(t, w)
  }

  /**
  * returns the heap cell another cell points to
  */
  fn get_ref(&self, x: Int) -> Int {
    self.heap[detag(x) as usize]
  }

  /**
  * sets a heap cell to point to another one
  */
  fn set_ref(&mut self, w: Int, r: Int) {
    self.heap[detag(w) as usize] = r
  }

  /**
   * removes binding for variable cells
   * above savedTop
   */
  fn unwind_trail(&mut self, saved_top: Int) {
    while saved_top < self.trail.len() as Int - 1 {
      let href = self.trail.pop().unwrap();
      self.set_ref(href, href);
    }
  }

  /**
  * scans reference chains starting from a variable
  * until it points to an unbound root variable or some
  * non-variable cell
  */
  fn deref(&self, mut x: Int) -> Int {
    while is_var(x) {
      let r = self.get_ref(x);
      if r == x {
        break;
      }
      x = r
    }
    x
  }

  fn add_sym(&mut self, s: String) -> Int {
    self.syms.memo(&s) as Int
  }

  fn put_clause(&mut self, cs: IntS, gs: &mut IntS, neck: Int) -> Clause {
    let base = self.size();
    let b = tag(V, base);
    let len = cs.len() as Int;

    self.push_cells2(b, 0, len, &cs);

    for i in 0 .. gs.len() {
      gs[i] = relocate(b, gs[i])
    }
    let mut xc = Clause {
      len: len,
      hgs: gs.clone(),
      base: base,
      neck: neck,
      xs: [0, 0, 0],
    };
    self.get_indexables(gs[0], &mut xc);
    xc
  }
  pub fn size(&self) -> Int {
    self.top + 1
  }

  /*
  * Pushes an element - top is incremented first than the
  * element is assigned. This means top point to the last assigned
  * element - which can be returned with peek().
  */
  fn push(&mut self, i: Int) {
    self.top += 1;
    self.heap[self.top as usize] = i;
  }

  /** runtime areas:
  *
  * the heap contains code for clauses and their copies
  * created during execution
  *
  * the trail is an undo list for variable bindings
  * that facilitates retrying failed goals with alternative
  * matching clauses
  *
  * the unification stack ustack helps handling term unification non-recursively
  *
  * the spines stack contains abstractions of clauses and goals and performs the
  * functions of both a choice-point stack and goal stack
  *
  * imaps: contains indexes for up to MAXIND>0 arg positions (0 for pred symbol itself)
  *
  * vmaps: contains clause numbers for which vars occur in indexed arg positions
  */
  fn make_heap(&mut self, size: Int) {
    self.heap = IntS::new();
    self.heap.resize(size as usize, 0);
    self.clear();
  }
  fn clear(&mut self) {
    self.top = -1
  }

  /**
  * copies and relocates head of clause at offset from heap to heap
  */
  fn push_head(&mut self, b: Int, c: &Clause) -> Int {
    self.push_cells1(b, 0, c.neck, c.base);
    relocate(b, c.hgs[0])
  }

  /**
  * copies and relocates body of clause at offset from heap to heap
  * while also placing head as the first element of array gs that
  * when returned contains references to the toplevel spine of the clause
  */
  fn push_body(&mut self, b: Int, head: Int, c: &Clause) {
    self.push_cells1(b, c.neck, c.len, c.base);
    let l = c.hgs.len();
    self.gs_push_body.resize(l, -1);
    self.gs_push_body[0] = head;
    for k in 1 .. l {
      let cell = c.hgs[k];
      self.gs_push_body[k] = relocate(b, cell);
    }
  }

  /**
  * pushes slice[from,to] of array cs of cells to heap
  */
  fn push_cells1(&mut self, b: Int, from: Int, to: Int, base: Int) {
    self.ensure_size(to - from);
    for i in from .. to {
      self.push(relocate(b, self.heap[(base + i) as usize]))
    }
  }
  fn push_cells2(&mut self, b: Int, from: Int, to: Int, cs: &IntS) {
    self.ensure_size(to - from);
    for i in from .. to {
      self.push(relocate(b, cs[i as usize]))
    }
  }

  fn ensure_size(&mut self, more: Int) {
    if 1 + self.top + more >= self.heap.len() as Int {
      self.expand()
    }
  }
  fn expand(&mut self) {
    self.heap.resize(self.heap.len() * 2, 0)
  }

  fn get_indexables(&self, ref_: Int, c: &mut Clause) {
    let p = 1 + detag(ref_);
    let n = detag(self.get_ref(ref_));
    let mut i = 0;
    while (i < MAXIND) && (i < n as usize) {
      let cell = self.deref(self.heap[(p as usize) + i]);
      c.xs[i] = self.cell2index(cell) as usize;
      i += 1
    }
  }

  fn make_index_args(&self, g: &mut Spine) {
    if g.xs[0] != 0 {
      return;
    }

    let goal = g.gs[0];
    let p = 1 + detag(goal);
    let n = std::cmp::min(MAXIND as Int, detag(self.get_ref(goal)));
    for i in 0 .. n {
      let cell = self.deref(self.heap[(p + i) as usize]);
      g.xs[i as usize] = self.cell2index(cell) as usize
    }
  }

  fn cell2index(&self, cell: Int) -> Int {
    match tag_of(cell) {
      R => self.get_ref(cell),
      C | N => cell,
      _ => 0,
    }
  }

  /**
  * transforms a spine containing references to choice point and
  * immutable list of goals into a new spine, by reducing the
  * first goal in the list with a clause that successfully
  * unifies with it - in which case places the goals of the
  * clause at the top of the new list of goals, in reverse order
  */
  fn unfold(&mut self) -> Option<Spine> {

    let mut g = self.spines[self.spines_top - 1].clone();
    let ttop = self.trail.len() as Int - 1;
    let htop = self.top;
    let base = htop + 1;

    self.make_index_args(&mut g);

    let last = g.cs.len() as Int;
    for k in g.k .. last {
      let c0 = self.clauses[g.cs[k as usize] as usize].clone();
      if !g.match_i(&c0) {
        continue
      }

      let base0 = base - c0.base;
      let b = tag(V, base0);
      let head = self.push_head(b, &c0);
      self.ustack.clear();
      self.ustack.push(head);
      self.ustack.push(g.gs[0]);
      if !self.unify(base) {
        self.unwind_trail(ttop);
        self.top = htop;
        continue;
      }

      self.push_body(b, head, &c0);
      g.k = k + 1;
      self.spines[self.spines_top - 1].k = g.k;

      if self.gs_push_body.len() > 1 || g.gs.len() > 1 {
        let p = self.gs_push_body.clone();
        let s = self.new_spine(&p, base, Some(&g.gs), ttop);
        return Some(s);
      } else {
        return Some(self.answer(ttop));
      }
    }
    None
  }

  /**
  * runtime representation of an immutable list of goals
  * together with top of heap and trail pointers
  * and current clause tried out by head goal
  * as well as registers associated to it
  *
  * note that parts of this immutable lists
  * are shared among alternative branches
  */
  fn new_spine(&mut self, gs0: &IntS, base: Int, rgs: Option<&IntList>, ttop: Int) -> Spine {
    if self.spines_top == self.spines.len() {
      self.spines.resize(self.spines.len() * 2, Spine::new())
    }

    let sp = &mut self.spines[self.spines_top];
    self.spines_top += 1;

    sp.hd = gs0[0];
    sp.cs = self.cls.clone(); // copy
    sp.base = base;
    sp.ttop = ttop;
    sp.xs[0] = 0;
    sp.xs[1] = 0;
    sp.xs[2] = 0;
    sp.k = 0;

    // note: cannot reuse G because the last spines.push_back could relocate the array
    let mut req_size = gs0.len() - 1;
    if let Some(x) = rgs {
      if x.len() > 0 {
        req_size = req_size + x.len() - 1
      }
    }

    sp.gs.resize(req_size, -1);
    let mut y = 0;
    if gs0.len() > 0 {
      for x in 1 .. gs0.len() {
        sp.gs[y] = gs0[x];
        y += 1
      }
    }
    if let Some(z) = rgs {
      if z.len() > 0 {
        for x in 1 .. z.len() {
          sp.gs[y] = z[x];
          y += 1
        }
      }
    }

    sp.clone()
  }

  /**
  * unification algorithm for cells X1 and X2 on ustack that also takes care
  * to trail bindigs below a given heap address "base"
  */
  fn unify(&mut self, base: Int) -> bool {
    while self.ustack.len() > 0 {
      let z1 = self.ustack.pop().unwrap();
      let z2 = self.ustack.pop().unwrap();
      let x1 = self.deref(z1);
      let x2 = self.deref(z2);
      if x1 != x2 {
        let w1 = detag(x1);
        let w2 = detag(x2);
        if is_var(x1) {
          if is_var(x2) && (w2 > w1) {
            self.heap[w2 as usize] = x1;
            if w2 <= base {
              self.trail.push(x2)
            }
          } else {
            self.heap[w1 as usize] = x2;
            if w1 <= base {
              self.trail.push(x1)
            }
          }
        } else if is_var(x2) {
          self.heap[w2 as usize] = x1;
          if w2 <= base {
            self.trail.push(x2)
          }
        } else if R == tag_of(x1) && R == tag_of(x2) {
          if !self.unify_args(w1, w2) {
            return false;
          }
        } else {
          return false;
        }
      }
    }
    true
  }

  fn unify_args(&mut self, w1: Int, w2: Int) -> bool {
    let v1 = self.heap[w1 as usize];
    let v2 = self.heap[w2 as usize];
    // both should be A
    let n1 = detag(v1);
    let n2 = detag(v2);
    if n1 != n2 {
      return false;
    }
    let b1 = 1 + w1;
    let b2 = 1 + w2;
    for i in (0 .. n1/* ?? - 1*/).rev() {
      let i1 = b1 + i;
      let i2 = b2 + i;
      let u1 = self.heap[i1 as usize];
      let u2 = self.heap[i2 as usize];
      if u1 == u2 {
        continue;
      }
      self.ustack.push(u2);
      self.ustack.push(u1)
    }
    true
  }

  /**
    * extracts a query - by convention of the form
    * goal(Vars):-body to be executed by the engine
    */
  fn get_query(&self) -> Clause {
    self.clauses.last().unwrap().clone()
  }

  /**
   * returns the initial spine built from the
   * query from which execution starts
   */
  pub fn init(&mut self) -> Spine {
    let base = self.size();
    let g = self.get_query();

    self.trail = IntStack::with_capacity(K_POOL as usize);
    self.ustack = IntStack::with_capacity(K_POOL as usize);

    self.spines.resize(K_POOL as usize, Spine::new());
    self.spines_top = 0;

    self.gs_push_body = IntS::with_capacity(K_PUSHBODY as usize);

    self.new_spine(&g.hgs, base, None, -1)
  }

  /**
   * returns an answer as a Spine while recording in it
   * the top of the trail to allow the caller to retrieve
   * more answers by forcing backtracking
   */
  fn answer(&mut self, ttop: Int) -> Spine {
    Spine::create(self.spines[0].hd, ttop)
  }

  /**
   * removes this spines for the spine stack and
   * resets trail and heap to where they where at its
   * creating time - while undoing variable binding
   * up to that point
   */
  fn pop_spine(&mut self) {
    self.spines_top -= 1;
    self.unwind_trail(self.spines[self.spines_top].ttop);
    self.top = self.spines[self.spines_top].base - 1
  }

  /**
   * main interpreter loop: starts from a spine and works
   * though a stream of answers, returned to the caller one
   * at a time, until the spines stack is empty - when it
   * returns null
   */
  fn yield_(&mut self) -> Option<Spine> {
    while self.spines_top > 0 {
      match self.unfold() {
        None => {
          self.pop_spine(); // no matches
          continue;
        }
        Some(cl) => {
          if cl.has_goals() {
            continue;
          }
          return Some(cl); // answer
        }
      }
    }
    None
  }

  /**
   * retrieves an answers and ensure the engine can be resumed
   * by unwinding the trail of the query Spine
   * returns an external "human readable" representation of the answer
   */
  pub fn ask(&mut self) -> Term {
    let y = self.yield_();
    match y {
      Some(q) => {
        let ans = self.answer(q.ttop);
        let res = ans.hd;
        let result = self.export_term(res);
        self.unwind_trail(q.ttop);
        self.query = None;
        result
      }
      _ => {
        Term::E
      }
    }
  }

  /**
    * builds an array of embedded arrays from a heap cell
    * representing a term for interaction with an external function
    * including a displayer
    */
  pub fn export_term(&self, xi: Int) -> Term {
    let x = self.deref(xi);
    let t = tag_of(x);
    let w = detag(x);
   
    match t {

      C => { RecObj::S(self.get_sym(w)) }
      N => { RecObj::I(w) }
      V => { RecObj::S(format!("V{}", w)) }
      R => {
        let a_ = self.heap[w as usize];
        if A != tag_of(a_) {
          panic!("*** should be A, found={}", self.show_cell(a_))
        }
        let n_ = detag(a_);
        //let mut args = Vec::<Box::<RecObj>>::new();
        let mut args = RecObjs::new();
        let k = w + 1;
        for i in 0 .. n_ {
          let j = k + i;
          args.push(Box::new(self.export_term(self.heap[j as usize])))
        }
        RecObj::A(args)
      }
      _ => { panic!("*BAD TERM* {}", self.show_cell(x)) }
    }
  }

  /**
    * returns the symbol associated to an integer index
    * in the symbol table
    */
  fn get_sym(&self, w: Int) -> String {
    self.syms[w as usize].clone()
  }

  /**
    * raw display of a cell as tag : value
    */
  pub fn show_cell(&self, w: Int) -> String {
    let t = tag_of(w);
    let val = detag(w);
    match t {
    V => { format!("v:{}", val) }
    U => { format!("u:{}", val) }
    N => { format!("n:{}", val) }
    C => { format!("c:{}", self.get_sym(val)) }
    R => { format!("r:{}", val) }
    A => { format!("a:{}", val) }
    _ => { format!("*BAD*={}", val) }
    }
  }
  pub fn show_cells2(&self, base: Int, len: Int) -> String {
    let mut buf = String::new();
    for k in 0 .. len {
      let instr = self.heap[(base + k) as usize];
      buf.push_str(&format!("[{}]{} ", base + k, self.show_cell(instr)));
    }
    buf
  }
/*
  pub fn show_cells1(&self, cs: &IntS) -> String {
    let mut result = String::new();
    for k in 0 .. cs.len() {
      result.push_str(&format!("[{}]{} ", k, self.show_cell(cs[k])));
    }
    result
  }

  pub fn pp_heap(&self) {
    println!("\nHEAP:\n\n{}\n",
      self.heap.iter().take(self.size() as usize).enumerate().map(|(i, x)|
        format!("[{}]{}", i, self.show_cell(*x))
      ).collect::<Vec<_>>().join("\n"))
  }
*/
}
/*
pub fn create_and_dload(asm: &str) {
  let e = Engine::new(asm);
  dbg!(e);
}

#[test]
pub fn test_create_and_dload() {
  create_and_dload("/home/carlo/develop/hhprolog/test/add.pl.nl")
}
*/
