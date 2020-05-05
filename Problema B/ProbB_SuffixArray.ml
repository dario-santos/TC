let dna, size1, size = 
  let dna1, n = let s = read_line()^"#" in s, String.length s in
  let dna     = let s = read_line()^"$" in dna1^s in
  dna, n, (String.length dna)


let get arr sz i = if i < sz then arr.(i) else -1


let construct_suffix_array dna n =
  let a = Array.init n (fun i -> Char.code dna.[i]) in
  let indexs = Array.make n ((0,0), 0) in
  let get_a = get a n in
  (* 
    Vamos ordenando aumentando por um fator de 2 a cada fase.
    2^0, 2^1, 2^2, ..., 2^n.
  *)
  let rec phase ammount =
    (* Atualizamos os valores em indexs *)
    for i = 0 to n-1 do
      indexs.(i) <- ((a.(i), get_a (i+ammount)), i)
    done;
    (* Ordena os indexs, utiliza um dos algoritmos: Merge Sort ou Heap Sort. *)
    Array.fast_sort compare indexs;

    a.(snd indexs.(0)) <- 0;

    for i = 1 to n-1 do
      let range_1, key_1 = indexs.(i-1) in
      let range_2, key_2 = indexs.(i) in

      a.(key_2) <- a.(key_1) + (if range_1 = range_2 then 0 else 1)
    done;
    
    if a.(snd indexs.(n-1)) < n-1 then phase (2*ammount)
  in
  phase 1;
  Array.init n (fun i -> snd indexs.(i))


let calculate_lcp sa = 
  let lcp = Array.make size 0 in
  let inv = Array.make size 0 in
  Array.iteri (fun i el ->  inv.(el) <- i) sa;

  let len = ref 0 in
  for i = 0 to size-1 do
    if inv.(i) > 0 then(
      (* Vamos buscar o suffixo anterior *)
      let j = sa.(inv.(i) - 1) in
      
      (* Enquanto houver texto e for igual incrementa *)
      while ((i + !len < size) && (j + !len < size) && dna.[i + !len] == dna.[j + !len]) do
        len := !len + 1
      done;
      
      (* Guarda o tamanho prefixo *)
      lcp.(inv.(i)) <- !len;
      
      if !len > 0 then len := !len - 1)
  done;
  lcp


let iterate sa lcp owners_map = 
  let n = String.length dna in
  
  let biggest = ref 0 in

  let high, low = ref 0, ref 0 in

  let has_dna1 = ref (owners_map.(0) = 1) in
  let has_dna2 = ref (not !has_dna1) in

  (* Do inicio ao fim, podia ser true, o segundo while vai lançar uma exceção sempre *)
  try(while !high <= n do
    (* Enquanto não temos as duas strings presentes *)
    while not (!has_dna1 && !has_dna2) do
      (* Vai causar uma exceção se não houver mais ocorrências *)
      high := !high + 1;
    
      if not !has_dna1 then has_dna1 := owners_map.(!high) = 1;
      if not !has_dna2 then has_dna2 := owners_map.(!high) = 2
    done;
    (* Será que só precisamos de verificar a última posição adicionada? 
       Penso que sim, pois a suffix tree está ordenada alfabéticamente e o LCP
       é em comparação com o elemento anterior. O high vai sempre ficar 
       com o valor da string que acabou de ser adicionada (que antes não existia)
       logo é a única possibilidade de haver uma comparação entre as duas strings.
    *)
    if lcp.(!high) > !biggest then biggest := lcp.(!high);
    (* Diminui a janela até só termos uma das string presentes *)
    low := !high;
    (* Reseta as flags *)
    has_dna1 := owners_map.(!high) = 1;
    has_dna2 := not !has_dna1
  done;
  !biggest
  )with _ -> !biggest


let sa  = construct_suffix_array dna size
let lcp = calculate_lcp sa
let owners_map = 
  Array.init (String.length dna) (fun i -> if sa.(i) < size1 then 1 else 2)


let _ = Printf.printf "%d\n" (iterate sa lcp owners_map)

(*
  Esta abordagem resolve o bigtestB (100_000 * 100_000) em 1s~2s.

  Havia várias abordagens para o cálculo do LCS entre duas strings,
  desde a programação dinamica a suffix trees e suffix arrays. 
    Tentámos resolver este problema clássico com a utilização de suffix trees,
  mas acabámos por optar pelas suffix arrays pois o seu algoritmo de construção é 
  mais simples do que o das suffix trees (Ukkonen).

  A nossa abordagem tem 3 partes distintas:
   1 - A construção da suffix array;
   2 - O cálculo do maior prefixo (LCP) - Kasai algorithm;  
   3 - A pesquisa do maior prefixo entre as duas strings, utiliza a técnica de sliding window.

  Referências:
  - https://www.youtube.com/watch?v=_TUeAdu-U_k
  - http://www.mi.fu-berlin.de/wiki/pub/ABI/RnaSeqP4/suffix-array.pdf
  - https://www.cs.cmu.edu/~15451-f18/lectures/lec26-suffarray.pdf
  - https://www.youtube.com/watch?v=Ic80xQFWevc
  - https://www.youtube.com/watch?v=DTLjHSToxmo
  - https://www.youtube.com/watch?v=53VIWj8ksyI
*)
