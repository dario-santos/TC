let dna1, n = let s = read_line() in s, String.length s
let dna2, m = let s = read_line() in s, String.length s
let matriz = Array.make_matrix n m 0
let min = ref 0

let get i j = try matriz.(i-1).(j-1) + 1 with _ -> 1
let operation i c =
  for j = 0 to m - 1 do
    if c = dna2.[j] then matriz.(i).(j) <- get i j;
    if matriz.(i).(j) > !min then min := matriz.(i).(j)
  done

let _ = 
  match (n,m) with
  | 0, _ | _, 0 -> ()
  | _ -> String.iteri (fun i c -> operation i c) dna1;
  Printf.printf "%d\n" !min

let debug () =
  for i = 0 to n-1 do
    for j = 0 to m-1 do
      Printf.printf "%d " matriz.(i).(j)
    done;
    Printf.printf "\n";
  done

(*let _ = debug ()*)

(*
  o segundo problema trata de descobrir qual é a maior parte comum entre dois ramos de ADN

   ABCABC
  A100100
  B020020
  C003003
  
  Função de inicialização:

  Se dna1.[i] = dna2.[j] então matriz.(i).(j) <- matriz(i-1).(j-1) + 1

  Input: 
  ABC
  ABCABC

  Output:
  2
*)