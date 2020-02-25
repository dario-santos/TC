(* O algoritmo implementado é o algoritmo utilizado na construção da tabela *)
(* De notar que o algoritmo aqui implementado é baseado no livro Programming Challenges (Presente na Biblioteca) *)
(* Site da wikipedia do algoritmo 'Levenshtein edit distance' *)

let dna1, n = let s =  "_" ^ (read_line()) in s, (String.length s)
let dna2, m = let s =  "_" ^ (read_line()) in s, (String.length s)

let matriz = Bigarray.Array2.create Bigarray.int Bigarray.c_layout n m
let _ = Bigarray.Array2.fill matriz max_int
let _ = matriz.{0, 0} <- 0

let debug () = 
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
        Printf.printf "%d" matriz.{i, j};
      done;
      Printf.printf "\n";      
  done

let get i j = 
  let i = if i < 0 then 0 else i in
  let j = if j < 0 then 0 else j in
  matriz.{i, j}

let min i j =
  let m = if get i (j-1) <= get (i-1) (j-1) then get i (j-1) else get (i-1) (j-1) in
  if m <= get (i-1) j then m else get (i-1) j

let operation dna1 dna2 n m = 
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
        matriz.{i, j} <- if dna1.[i] = dna2.[j] then get (i-1) (j-1) else (min i j) + 1;
      done;
  done

let main dna1 dna2 n m =
  if n == 0 then m
  else if m == 0 then n 
  else let _= operation dna1 dna2 n m in matriz.{n - 1, m - 1}

let _ = Printf.printf "%d\n" (main dna1 dna2 n m)
