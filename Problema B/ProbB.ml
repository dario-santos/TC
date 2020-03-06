let dna1, dna2 = read_line(), read_line()
let n, m = String.length dna1, String.length dna2
let matriz = Array.make_matrix n m 0

let min, out = ref 0, ref ""

let operation i a = 
  for j = 0 to m - 1 do
    let tmp = if i - 1 < 0 || j - 1 < 0 then 0 else matriz.(i-1).(j-1) in
    (if dna1.[i] = dna2.[j] then matriz.(i).(j) <- tmp + 1); 
    if matriz.(i).(j) > !min then
      (min := matriz.(i).(j);
      out := string_of_int !min)
    else if matriz.(i).(j) = !min then
      out := !out ^ " " ^ (string_of_int !min)
  done

let main = function
  | 0, _ | _, 0 -> "0"
  | _ -> 
    String.iteri (fun i a -> operation i a) dna1;
    !out

let _ = Printf.printf "%s\n" (main (n,m))

let debug () = 
  for i = 0 to n-1 do
    for j = 0 to m-1 do
      Printf.printf "%d " matriz.(i).(j)
    done;
    Printf.printf "\n"
  done

let _ = debug()

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
  2 2
*)