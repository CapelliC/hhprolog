use std::collections::BTreeMap;

pub type Int = isize;

pub type IntS = Vec<Int>;
pub type IntList = IntS;
pub type IntStack = IntS;

pub type Cardinal = usize;

pub type TRefs = BTreeMap<String, Vec<usize>>;
pub trait Refs {
  fn at(&mut self, k: String) -> &mut Vec<usize>;
}
impl Refs for TRefs {
  fn at(&mut self, k: String) -> &mut Vec<usize> {
    self.entry(k).or_default()
  }
}

#[test]
fn test_1() {
  let mut x = TRefs::new();
  let k = "x".to_owned();
  x.insert(k.clone(), vec![1,2,3]);
  x.entry(k.clone()).or_default().push(4);
  x.at(k.clone()).push(6);
  dbg!(x);
}

pub const MINSIZE:usize = 1 << 15;
//pub const START_INDEX:usize = 20;

pub const MAXIND:usize = 3;
pub type TXs = [usize; MAXIND];

#[derive(Debug, Clone)]
pub struct Clause {
  pub len: Int,
  pub hgs: IntS,
  pub base: Int,
  pub neck: Int,
  pub xs: TXs
}

#[derive(Debug, Clone)]
pub struct Spine {

  pub hd: Int,      // head of the clause to which this corresponds
  pub base: Int,    // top of the heap when this was created

  pub gs: IntList,  // goals - with the top one ready to unfold
  pub ttop: Int,    // top of the trail when this was created

  pub k: Int,

  pub xs: TXs,      // index elements
  pub cs: IntS,     // array of clauses known to be unifiable with top goal in gs
}

impl Spine {
  pub const fn new() -> Self {
    Spine {
      hd: 0,
      base: 0,
      gs: IntList::new(),
      ttop: 0,
      k: 0,
      xs: [0,0,0],
      cs: IntS::new()
    }
  }
  pub const fn create(hd: Int, ttop: Int) -> Self {
    let mut r = Self::new();
    r.hd = hd;
    r.ttop = ttop;
    r
  }
  /*
  pub fn has_clauses(&self) -> bool {
    self.k < self.cs.len() as isize
  }
  */
  pub fn has_goals(&self) -> bool {
    self.gs.len() > 0
  }
}
