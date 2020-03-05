(* o segundo problema trata de descobrir qual é a maior parte comum entre dois ramos de ADN *)
let dna1, dna2 = read_line(), read_line()
let n, m = String.length dna1, String.length dna2

let matriz = Array.make_matrix n m 0

let get i j = if i - 1 < 0 || j - 1 < 0 then 0 else matriz.(i-1).(j-1)

let init () = 
  for i = 0 to n-1 do
    for j = 0 to m-1 do
      if dna1.[i] = dna2.[j] then 
        matriz.(i).(j) <- (get i j) + 1
    done;
  done

let debug () = 
  for i = 0 to n-1 do
    for j = 0 to m-1 do
      Printf.printf "%d " matriz.(i).(j)
    done;
    Printf.printf "\n"
  done

let main() =
  init();
  let a = ref 0 in
  let b = ref 0 in
  let k = ref 0 in
  for i = 0 to n-1 do
    for j = 0 to m-1 do
      if matriz.(i).(j) > !k then 
        k := matriz.(i).(j);
        a := i;
        b := j;
    done;
  done;
  !a,!b,!k

(* 
  a = Inicio do padrão DNA1
  b = Inicio do padrão DNA2 
  k = Tamanho do padrão
  Output: a b k
*)
let _ = let a,b,k = main() in Printf.printf "%d %d %d\n" a b k

(*

   BAAAA
  A01111
  A01222
  B10000
  B10000

  Função de inicialização:

  Se dna1.[i] = dna2.[j] então matriz.(i).(j) <- matriz(i-1).(j-1) + 1


*)