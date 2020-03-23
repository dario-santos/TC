let dna1, n = let s = read_line() in s, String.length s
let dna2, m = let s = read_line() in s, String.length s
let matriz = Array.make_matrix 2 n 0
let min = ref 0

let iterate arr c =
  for j = 0 to n-1 do
    if c = dna1.[j] then
      (arr.(j) <- (try arr.(j-1) + 1 with _ -> 1);
      if arr.(j) > !min then min := arr.(j))
  done

let _ =
  (match (n,m) with
  | 0, _ | _, 0 -> ()
  | _ when String.equal dna1 dna2 -> min := n
  | _ -> String.iteri (fun i c -> iterate matriz.(i mod 2) c) dna2);
  Printf.printf "%d\n" !min

(*
  # Problema A e Problema B

  À semelhança do problema A, o problema B também pede por uma resposta ótima.
  Logo utilizámos programação dinamica para resolver este problema de forma eficiente.

  Também à semelhança do problema A, alocámos apenas uma array com 2n elementos.

  # Algoritmo

    |A|C|A|A|T|
  --+-+-+-+-+-+
   C|0 1 0 0 0
   A|1 0 2 1 0
   T|0 0 0 0 2
  
  # Algumas Considerações:

  Neste caso temos duas situações que produzem o mesmo tamanho máximo: 
    
    - dna1: CA,  dna2: ACA
    - dna1: CAT, dna2: ACAAT

  Visto que não importa qual o que retornamos, foi decidido retornar o primeiro
  de forma a otimizar o algoritmo.

*)
