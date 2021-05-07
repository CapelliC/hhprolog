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
