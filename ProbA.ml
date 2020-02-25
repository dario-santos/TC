open Bigarray.Array2
let dna1 = "ola"
let dna2 = "adeus"
let matriz = Bigarray.Array2.create Bigarray.int Bigarray.c_layout (String.length dna1) (String.length dna2)

(* 
 *     |i-1| i |
 *     +---+---+ 
 * j-1 | 1 | 2 |
 *     +---+---+
 *  j  | 3 | x |
 *     +---+---+
 * *)
let min i j =
  let min = if matriz.{i, j - 1} <= matriz.{i - 1, j-1} then matriz.{i, j - 1} else matriz.{i - 1, j -1 } in
  if min <= matriz.{i - 1, j} then min else matriz.{i - 1, j}

let calculate dna1 dna2 =
  (*Colocar aqui o corpo*)


let main dna1 dna2 =
  (* Verifica se existe uma vazia *)
  if String.length dna1 == 0 then String.length dna2 
  else if String.length dna2 == 0 then String.length dna1 
  else (preenche matriz dna1 dna2) 
    
let _ = Printf.printf "%d\n" (main dna1 dna2)
