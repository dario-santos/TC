use std::*;

fn input() -> Vec<char>
{
  let mut s = String::new();
  io::stdin().read_line(&mut s).expect("Errer while reading input.");
  s.chars().collect()
}

fn main() 
{
  let dna1 = input();
  let dna2 = input();
  
  let mut matrix = vec![vec![0; dna1.len()]; dna2.len()];

  let mut max = 0;

  for i in 1..dna2.len()
  {
    for j in 1..dna1.len()
    {
      if dna1[j-1] == dna2[i-1] 
      {
        matrix[i][j] = matrix[i-1][j-1] + 1;
        max = if matrix[i][j] >= max {matrix[i][j]} else {max};
      }
    }
  }

  println!{"{}", max};
}
