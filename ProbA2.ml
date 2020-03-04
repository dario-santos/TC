let dna1 = read_line()
let dna2 = read_line()

let min a b c =
  let m = if a < b then a else b in 
  if m < c then m else c

let main dna1 dna2 =
  let n, m = String.length dna1, String.length dna2 in
  let matriz = Array.make_matrix 2 (n + 1) 0 in
  
  for i = 0 to n do
    matriz.(0).(i) <- i;
  done;

  for i = 1 to m do
    for j = 0 to n do
      let a = i mod 2 in
      let b = (i - 1) mod 2 in
      if j = 0 then matriz.(a).(j) <- i
      else if dna1.[j - 1] = dna2.[i - 1] then matriz.(a).(j) <- matriz.(b).(j - 1)
      else matriz.(a).(j) <- 1 + (min matriz.(b).(j) matriz.(a).(j - 1) matriz.(b).(j - 1))
    done;
  done;
  matriz.(m mod 2).(n)

let _ = Printf.printf "%d\n" (main dna1 dna2)
