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
      RecObj::E     => "$null".to_string(),
      RecObj::I(i)  => i.to_string(),
      RecObj::S(s)  => s.clone(), //format!("\"{}\"", s),
      RecObj::A(a)  => format!("({})", a.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", ")),
    }
  }
}

use std::fmt;
impl fmt::Display for RecObj {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.to_string())
  }
}
