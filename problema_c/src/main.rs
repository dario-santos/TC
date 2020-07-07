//
mod parser;

use std::collections::HashMap;

fn main() {
    let n = parser::read_int();
    let mut productions: HashMap<char, Vec<Vec<parser::Value>>> = HashMap::new();

    for _ in 0..n {
        let (non_terminal, production) = parser::input();

        match productions.get_mut(&non_terminal) {
            Some(p) => p.push(production),
            None => {productions.insert(non_terminal, vec![production]);}
        }
    }

    debug(&productions);
}

fn null(productions: &HashMap<char, Vec<Vec<parser::Value>>>) -> HashMap<char, bool> {
    let mut visitados : Vec<(char, Vec<parser::Value>)> = Vec::new();

    fn get_null(non_terminal : char)
    {
        let p = productions.get(non_terminal);
        for p in 
    }
}

fn debug(productions: &HashMap<char, Vec<Vec<parser::Value>>>) {
    for (k, v) in productions.iter() {
        println!("{} = {:?}", k, v);
    }
}
