(* https://www.youtube.com/watch?v=We3YDTzNXEk&list=PLLXdhg_r2hKA7DPDsunoDZ-Z769jWn4R8 *)
(* O algoritmo implementado é o algoritmo utilizado na construção da tabela *)

let dna1 = read_line()
let dna2 = read_line()

let array = Bigarray.Array2.create Bigarray.int Bigarray.c_layout (String.length dna1) (String.length dna2)

let init n m  = 
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      array.{i, j} <- max_int;
    done;
  done;
  array.{0, 0} <- 0

let min i j =
  let a =  if i - 1 <= 0 then 0 else i - 1 in
  let b =  if j - 1 <= 0 then 0 else j - 1 in
  let min = if array.{i, b} <= array.{a, b} then array.{i, b} else array.{a, b} in
  if min <= array.{a, j} then min else array.{a, j}


let operacao dna1 dna2 =
  let n = String.length dna1 in
  let m = String.length dna2 in
  init n m;
  if n == 0 then m
  else if m == 0 then n 
  else 
    begin
      for i = 0 to n - 1 do
        for j = 0 to m - 1 do
           array.{i, j} <- if dna1.[i] = dna2.[j] then (min i j) else (min i j) + 1;
        done;
      done;
      array.{n - 1, m - 1}
    end

let _ = Printf.printf "%d\n" (operacao dna1 dna2)
