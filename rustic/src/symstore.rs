pub trait SymStore {
  fn memo(&mut self, s: &str) -> usize;
}

impl SymStore for Vec<String> {
  fn memo(&mut self, s: &str) -> usize {
    match self.iter().position(|r| r == s) {
      Some(q) => q,
      None => {
        self.push(s.to_string());
        self.len() - 1
      }
    }
  }
}

#[test]
pub fn test_2() {
  let mut s = Vec::<String>::new();

  let x = s.memo("x");
  assert_eq!(x, 0);

  let y = s.memo("y");
  assert_eq!(y, 1);

  let a = s.memo("a");
  assert_eq!(a, 2);

  let x1 = s.memo("x");
  assert_eq!(x, x1);
}
