mod prog;
mod engine;
mod plscan;
mod recobj;
mod hhvector;
mod symstore;
mod base_types;

fn main() {
  let mut src = "add".to_string();
  let a = std::env::args().collect::<Vec<_>>();
  if a.len() >= 2 {
    src = a[1].clone()
  }

  let f = format!("/home/carlo/develop/hhprolog/test/{}.pl.nl", src);
  let mut e = engine::Engine::new(&f);

  e.pp_code();

  let print_sol = a.len() >= 3 && a[2] == "true";
  e.run(print_sol);
}
