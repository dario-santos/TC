// S -> a b S
// D -> d
// C -> c F g
// A -> _
// 0123

// 0       -> Não terminal
// 1..3    -> Seta
// 4..'\n' -> produção

use std::*;

pub enum Value {
    E(),
    T(char),
    N(char),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let out = {
            match self {
                Value::E() => &'_',
                Value::T(c) => c,
                Value::N(c) => c,
            }
        };

        f.debug_struct("").field("c ", out).finish()
    }
}

pub fn read_int() -> i32 {
    let mut s = String::new();
    io::stdin()
        .read_line(&mut s)
        .expect("Failed to read from stdin");

    s.trim().parse::<i32>().unwrap()
}

pub fn input() -> (char, Vec<Value>) {
    let mut s = String::new();
    io::stdin()
        .read_line(&mut s)
        .expect("Failed to read from stdin");

    let buffer: Vec<char> = s.chars().collect();

    let non_terminal = buffer[0];
    let mut production: Vec<Value> = Vec::new();

    for c in buffer.iter().skip(4) {
        match c {
            '\n' => break,
            ' ' => continue,
            'a'..='z' => production.push(Value::T(*c)),
            'A'..='Z' => production.push(Value::N(*c)),
            '_' => production.push(Value::E()),
            _ => (),
        }
    }

    (non_terminal, production)
}
