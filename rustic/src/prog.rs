use crate::engine::Engine;
use crate::base_types::{Clause, Int};

use crate::recobj::*;
use RecObj::{E};

type Term = RecObj;
type Terms = RecObjs;

fn maybe_null(o: &Term) -> String {
  match o {
    Term::E => "$null".to_string(),
    Term::A(a) => st0(a),
    _ => o.to_string()
  }
}
fn is_list_cons(name: &str) -> bool {
  "." == name || "[|]" == name || "list" == name
}
fn is_op(name: &str) -> bool {
  "/" == name || "-" == name || "+" == name || "=" == name
}

fn st0(args: &Terms) -> String {
  let mut r = String::new();
  if args.len() > 0 {
    let name = format!("{}", args[0]);
    if args.len() == 3 && is_op(&name) {
      r += "(";
      r += &maybe_null(&*args[0]);
      r += " "; r += &name; r += " ";
      r += &maybe_null(&*args[1]);
      r += ")"
    }
    else if args.len() == 3 && is_list_cons(&name) {
      r += "[";
      r += &maybe_null(&*args[1]);
      let mut tail = &*args[2];
      loop {
        if ("[]" == tail.to_string()) || ("nil" == tail.to_string()) {
          break
        }
        match tail {
          Term::A(list) => {
            if !( list.len() == 3 && is_list_cons(&*list[0].to_string()) ) {
              r += "|";
              r += &maybe_null(tail);
              break
            }
            else {
              r += ",";
              r += &maybe_null(&*list[1]);
              tail = &*list[2]
            }
          },
          _ => {
            r += "|";
            r += &maybe_null(tail);
            break
          }
        }
      }

      r += "]"
    }
    else if (args.len() == 2) && ("$VAR" == name) {
      r += "_";
      r += &*args[1].to_string()
    }
    else
    {
      let qname = maybe_null(&*args[0]);
      r += &qname;
      r += "(";
      for i in 1 .. args.len() {
        r += &maybe_null(&*args[i]);
        if i < args.len() - 1 {
          r += ","
        }
      }
      r += ")"
    }
  }
  r
}

impl Engine {
  pub fn run(&mut self, print_ans: bool) {
    let mut ctr = 0;
    let now = std::time::Instant::now();
    loop {
      let a = self.ask();
      if let E = a {
        break
      }
      ctr += 1;
      if print_ans {
        println!("{} *** ANSWER={}", ctr, self.show_term_o(&a))
      }
    }
    println!("TOTAL ANSWERS={} in {} msec", ctr, now.elapsed().as_millis())
  }
  
  pub fn pp_code(&self) {
    println!("\nSYMS:\n\n{{{}}}",
      self.syms.iter().enumerate().map(|(i, x)|
        format!("{}={}", x, i)
      ).collect::<Vec<_>>().join(", "));

    println!("\nCLAUSES:\n\n{}\n",
      self.clauses.iter().enumerate().map(|(i, x)|
        format!("[{}]{}", i, self.show_clause(x))
      ).collect::<Vec<_>>().join("\n"))
  }

  fn show_clause(&self, s: &Clause) -> String {
    
    let l = s.hgs.len();
    
    let mut r = format!(":---base:[{}] neck: {}-----\n", s.base, s.neck);
    r.push_str(&self.show_cells2(s.base, s.len)); r += "\n";
    r.push_str(&self.show_cell(s.hgs[0]));
    r.push_str(" :- [");
    for i in 1 .. l {
      r.push_str(&self.show_cell(s.hgs[i]));
      if i < l - 1 {
        r.push_str(", ");
      }
    }
    r.push_str("]\n");
  
    r.push_str(&self.show_term_i(s.hgs[0]));
    if l > 1 {
      r.push_str(" :- \n");
      for i in 1 .. l {
        r.push_str("  ");
        r.push_str(&self.show_term_i(s.hgs[i]));
      }
    }
    r.push_str("\n");
    
    r
  }
  fn show_term_o(&self, o: &Term) -> String {
    match o {
    Term::I(i)  => self.show_term_i(*i),
    Term::A(a)  => st0(a),
    Term::S(s)  => s.clone(),
    _           => o.to_string()
    }
  }
  fn show_term_i(&self, x: Int) -> String {
    let t = self.export_term(x);
    self.show_term_o(&t)
  }

}
