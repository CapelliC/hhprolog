#[derive(Debug, PartialEq, Eq)]
pub enum PlTok<'a> {
	//SPACE(String),
	ATOM(&'a str),
	VAR(&'a str),
	NUM(isize),
	//NUM(&'a str),
	DOT,
	AND,
	HOLDS,
	IF,
	IS,
	LISTS,
}

fn tokenizer<'t>() -> regex_lexer::Lexer<'t, PlTok<'t>> {
	regex_lexer::LexerBuilder::new()
		.token(r"-?\d+", |s| Some(PlTok::NUM(s.parse().unwrap())))
		.token(r"[A-Z_]\w*", |s| Some(PlTok::VAR(s)))
		.token(r"[a-z]\w*", |s| Some(PlTok::ATOM(s)))
		.token(r"\s+", |_| None)
		.token(r"\.", |_| Some(PlTok::DOT))
		.token(r"and", |_| Some(PlTok::AND))
		.token(r"holds", |_| Some(PlTok::HOLDS))
		.token(r"if", |_| Some(PlTok::IF))
		.token(r"is", |_| Some(PlTok::IS))
		.token(r"lists", |_| Some(PlTok::LISTS))
		.build()
		.unwrap()
}

type Ts = Vec<String>;
type Tss = Vec<Ts>;
type Tsss = Vec<Tss>;

pub fn to_sentences(s: &str) -> Tsss {
	let mut wsss = Tsss::new();
	let mut wss = Tss::new();
	let mut ws = Ts::new();

	/*
	fn c(k: char) -> String {
			format!("{}:{}", k, ws[0].chars().skip(2).collect::<String>())
	}
	*/
	//let mut c = |k| format!("{}:{}", k, ws[0].chars().skip(2).collect::<String>());
	//let s2 = move || ws[0].chars().skip(2).collect::<String>();
	/*fn s2(s: &String) -> String {
			s.chars().skip(2).collect::<String>()
	}*/

	let s2 = |s: &String| s.chars().skip(2).collect::<String>();
	let p2 = |c: char, s: &str| format!("{}:{}", c, s);

	for t in tokenizer().tokens(s) {
		match t {
			PlTok::DOT => {
				wss.push(ws);
				wsss.push(wss);
				wss = Tss::new();
				ws = Ts::new()
			}
			PlTok::IF => {
				wss.push(ws);
				ws = Ts::new()
			}
			PlTok::AND => {
				wss.push(ws);
				ws = Ts::new()
			}
			PlTok::HOLDS => ws[0] = p2('h', &s2(&ws[0])),
			PlTok::LISTS => ws[0] = format!("l:{}", s2(&ws[0])),
			PlTok::IS => ws[0] = format!("f:{}", s2(&ws[0])),
			PlTok::ATOM(v) => ws.push(p2('c', &v)),
			PlTok::VAR(v) => ws.push(p2('v', &v)),
			PlTok::NUM(n) => ws.push(format!("{}:{}", if n < (1 << 28) { "n" } else { "c" }, n)),
		}
	}
	wsss
}

#[test]
pub fn test_3() {
	fn tokenize(s: &str) -> Vec<PlTok> {
		tokenizer().tokens(s).collect()
	}

	let t = tokenize("if A and b listsx lists -123");
	dbg!(&t);
	assert_eq!(
		t,
		vec![
			PlTok::IF,
			PlTok::VAR("A"),
			PlTok::AND,
			PlTok::ATOM("b"),
			PlTok::ATOM("listsx"),
			PlTok::LISTS,
			PlTok::NUM(-123),
		]
	);
	dbg!(&t);
}

#[test]
pub fn test_4() {
	let g = to_sentences(
		r"
    add 0 X X .
    add 100 x z .
    ",
	);
	dbg!(g);
}

pub fn sub_str(s: &str, b: usize, e: Option<usize>) -> String {
	match e {
		Some(y) => s.chars().skip(b).take(y).collect::<String>(),
		None => s.chars().skip(b).collect::<String>(),
	}
}

/**
	* expands a "Xs lists .." statements to "Xs holds" statements
	*/
pub fn maybe_expand(ws: &Ts) -> Tss {
	let w = &ws[0];
	let v: String;
	let mut vi: String;
	let mut vii: String;
	let mut rs: Ts;

	let mut result = Tss::new();
	if (w.len() < 2) || ("l:" != sub_str(&w, 0, Some(2))) {
		return result;
	}

	let n = ws.len();
	v = sub_str(&w, 2, None);
	rs = Ts::new();
	for i in 1 .. n {
		if 1 == i {
			vi = v.clone()
		} else {
			vi = format!("{}__{}", v, i - 1)
		}
		vii = format!("{}__{}", v, i);
		if i == n - 1 {
			let t = &mut vec![
				"h:".to_string() + &vi,
				"c:list".to_string(),
				ws[i].clone(),
				"c:nil".to_string(),
			];
			rs.append(t)
		} else {
			let t = &mut vec![
				"h:".to_string() + &vi,
				"c:list".to_string(),
				ws[i].clone(),
				"v:".to_string() + &vii,
			];
			rs.append(t)
		}
		result.push(rs.clone());
		rs = Ts::new()
	}
	result
}

/**
	* expands, if needed, "lists" statements in sequence of statements
	*/
pub fn map_expand(wss: &Tss) -> Tss {
	//let mut Hss: Tss;
	//let Ws: Ts;
	//let Cs: Ts;

	let mut result = Tss::new();
	for ws in wss {
		let hss = maybe_expand(&ws);
		if hss.len() == 0 {
			result.push(ws.to_vec())
		} else {
			for cs in hss {
				result.push(cs)
			}
		}
	}
	result
}
