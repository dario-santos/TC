use ::std::*;

fn input() -> Vec<char>
{
  let mut ret = String::new();
  io::stdin().read_line(&mut ret).expect("Failed to read from stdin");
  ret.chars().collect()
}

fn min(x: i32, y: i32, z: i32) -> i32
{
  let tmp = if x <= y {x} else {y};
  if tmp <= z {tmp} else {z}
}

fn main() 
{
  let dna1 = input();
  let dna2 = input();
  let mut matrix = vec![vec![0; dna1.len()]; dna2.len()];

  for i in 0..dna2.len()
  {
    matrix[i][0] = i as i32;
  }

  for j in 0..dna1.len()
  {
    matrix[0][j] = j as i32;
  }

  // Percorre a matriz
  for j in 1..dna2.len()
  {
    for i in 1..dna1.len()
    {   
      let m = min(matrix[j-1][i-1], matrix[j-1][i], matrix[j][i-1]);

      matrix[j][i] = if dna1[i-1] != dna2[j-1] {m + 1} else {matrix[j-1][i-1]};
    }
  }

  println!("{}", matrix[dna2.len()-1][dna1.len()-1]);
}
