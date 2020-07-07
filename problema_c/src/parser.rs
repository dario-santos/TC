

// S -> a b S
// D -> d
// C -> c F g
// A -> _
// 0123

// 0       -> Não terminal
// 1..3    -> Seta
// 4..'\n' -> produção

use std::*;

pub fn input()
{
  let mut buffer = String::new();
  io::stdin().read_to_string(&mut buffer).expect("Error while reading stdin.");

  let nonTerminal = buffer[0];


  for c in buffer.chars()
  {
    match c {
      '\n' => break,
      ' '  => continue,
      'a'..='z' => println!("TERMINAL"),
      'A'..='Z' => println!("NAO TERMINAL"),
      '_'      => println!("EPSILON"),
    }
  }
}