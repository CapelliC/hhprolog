mod prog;
mod engine;
mod plscan;
mod recobj;
mod hhvector;
mod symstore;
mod base_types;

use std::env;
use std::ffi::OsStr;

fn main() {
  let mut script = "add".to_string();
  let a = std::env::args().collect::<Vec<_>>();
  if a.len() >= 2 {
    script = a[1].clone()
  }

  let f = path_to_script(&script);
  let mut e = engine::Engine::new(&f);

  e.pp_code();

  let print_sol = a.len() >= 3 && a[2] == "true";
  e.run(print_sol);
}

// assume this crate got built somewhere nested in hhprolog path,
// and then it's run from a target forlder, so take cwd and go upward 'til matching hhprolog folder
fn path_to_script(script_no_ext: &String) -> String {

  // lookup hhprolog
  let hhprolog = OsStr::new("hhprolog");
  let mut path = env::current_dir().unwrap();
  while path.pop() {
    let top = path.iter().last().unwrap();
    if top == hhprolog {
      break
    }
  }

  // down into test
  path.push("test");
  path.push(script_no_ext.to_owned() + ".pl.nl");
  path.into_os_string().into_string().unwrap()
}
