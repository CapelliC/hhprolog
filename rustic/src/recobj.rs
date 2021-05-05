/*
#[derive(Debug, Clone)]
pub enum RecObj {
  Rt,
  RtI(isize),
  RtPs(String),
  RtVo(Vec<Box<RecObj>>)
}

impl RecObj {
  pub fn to_string(&self) -> String {
    match self {
      Rt => "?".to_owned(),
      RtI(i) => format!("{}", i),
      RtPs(s) => format!("\"{}\"", s),
      RtVo(v) => v.map(|e| e.to_string())
    }
  }
}
*/
pub type RecObjs = Vec<Box<RecObj>>;

#[derive(Debug, Clone)]
pub enum RecObj {
  E,
  I(isize),
  S(String),
  A(RecObjs)
}

impl RecObj {
  pub fn to_string(&self) -> String {
    match self {
      RecObj::E     => "-".to_string(), // "-".to_owned(),
      RecObj::I(i)  => i.to_string(), // format!("{}", i),
      RecObj::S(s)  => format!("\"{}\"", s),
      RecObj::A(a)  => format!("[{}]", a.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", ")),
    }
  }
}

use std::fmt;
impl fmt::Display for RecObj {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.to_string())
  }
}
