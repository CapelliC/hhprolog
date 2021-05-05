mod prog;
mod engine;
mod plscan;
mod recobj;
mod hhvector;
mod symstore;
mod base_types;

fn main() {
  //test_0();
  //test_1();
  //test_2();
  //test_3();
  //test_4();

  let f = "/home/carlo/develop/hhprolog/test/add.pl.nl";
  //create_and_dload(f);
  let mut e = engine::Engine::new(f);
  e.pp_code();
  e.run(true);
}
