pub trait HhVector<T> {
  fn slice(&self, s: usize) -> Vec<T>;
  fn concat(&self, s: &Vec<T>) -> Vec<T>;
  fn capacity(&self) -> usize;
}

impl<T:Copy> HhVector<T> for Vec<T> {

  fn slice(&self, s: usize) -> Vec<T> {
    let mut r = Vec::<T>::new();
    let mut i = s;
    while i < self.len() {
      r.push(self[i]);
      i += 1  // = i + 1
    }
    /*for i in 0 .. std::cmp::min(self.len(), s) {
      r.push(self[i])
    }*/
    r
  }

  fn concat(&self, s: &Vec<T>) -> Vec<T> {
    let mut r = self.clone();
    for e in s {
      r.push(e.clone())
    }
    r
  }

  fn capacity(&self) -> usize {
    self.len()
  }

}
