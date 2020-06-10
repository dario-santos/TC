(*
  Referências:
  - https://www.youtube.com/watch?v=_TUeAdu-U_k
  - http://www.mi.fu-berlin.de/wiki/pub/ABI/RnaSeqP4/suffix-array.pdf
  - https://www.cs.cmu.edu/~15451-f18/lectures/lec26-suffarray.pdf
  - https://www.youtube.com/watch?v=Ic80xQFWevc
  - https://www.youtube.com/watch?v=DTLjHSToxmo
  - https://www.youtube.com/watch?v=53VIWj8ksyI

  Autores:
    37283 Pedro Moreira
    39973 Dário Santos
*)


(* 
  Ler o input e concatenar as duas strings numa só.
  De forma a marcar o fim das duas strings são utilizados os dois caracteres especiais,
  # e $.
  Exemplo:
    Input:
    ACC
    CAT
  
    dna   = "AAC#CAT$"
    size1 = 4
    size2 = 8
*)
let dna, size1, size2 =
  let dna1, n = let s = read_line()^"#" in s, String.length s in
  let dna     = let s = read_line()^"$" in dna1^s in
  dna, n, (String.length dna)

(* 
Função de utilizade da construct_suffix_array.
Serve para construir a array de indices.
*)
let get arr sz i = if i < sz then arr.(i) else -1


(*
A função construct_suffix_array, constroi uma suffix array utilizando um algoritmo otimizado de ordenação,
a ideia deste algoritmo é de ordernarmos a suffix array primeiro pelos primeiros dois caracteres, na segunda iteração
ordenamos os primeiros quatro caracteres tirando proveito do que já está ordenando, etc. sempre aumentando com um factor de 2.

Uma suffix array não é nada mais do que uma array que contém todos os sufixos de uma palavra ordenados alfabéticamente.

Exemplo:
  palavra:  A A C # C A T $
            0 1 2 3 4 5 6 7

  suffix array:
    [3; 7; 0; 1; 5; 2; 4; 6]
  
Calcular os sufixos de uma palavra não é complicado, mas ordená-los alfabéticamente é. Logo necessitámos de 
utilizar um método de ordenação utilizado na construção das suffix arrays.

Começamos por atribuír a cada caracter do alfabeto um número único, neste caso utilizamos os códigos ASCII.

A nossa palavra "AAC#CAT$" passaria a ser representada por [65; 65; 67; 35; 67; 65; 84; 36].

Vamos agrupar os indices, agrupando o atual com o seguinte, vamos para além disso guardar a sua posição inicial para
que futuramente saibamos a posição original de cada tuplo.

Isto iria resultar da array de tuplos:
  [((65, 65), 0); ((65, 67), 1); ((67, 35), 2); ((35, 67), 3); ((67, 65), 4); ((65, 84), 5); ((84, 36), 6); ((36, -1), 7)].

podemos agora ordenar esta array:
  [((35, 67), 3); ((36, -1), 7); ((65, 65), 0); ((65, 67), 1); ((65, 84), 5); ((67, 35), 2); ((67, 65), 4); ((84, 36), 6)]

e atualizar os valores do vetor a, que irá à posição inicial de cada elemento e atualizar pela nova posição em que ficou depois de ordenado.
Existe um caso particular que acontece quando temos elementos repetidos, estes vão ser atualizados para o mesmo valor e como o array ficou
ordenado vão ficar contiguos logo só temos que verificar se o elemento anterior era igual ao atual. Se for incrementamos se não mantemos o mesmo.

  a = [2; 3; 5; 0; 6; 4; 7; 1]

Na segunda iteração, em vez de criarmos tuplos com os elementos seguintes, vamos criar elementos com 1 casa de intervalo. Na terceira iteração com três casas 
de intervalo e por aí a diante.


Isto é repetido até que se encontre no array o elemento (tamanhoDaString-1) o que significará que todos os elementos em a são únicos e o suffix array
estará ordenado.
*)
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

    (* Coloca o primeiro elemento a zero *)
    a.(snd indexs.(0)) <- 0;

    (* Vai atualizando a array a *)
    for i = 1 to n-1 do
      let range_1, key_1 = indexs.(i-1) in
      let range_2, key_2 = indexs.(i) in

      a.(key_2) <- a.(key_1) + (if range_1 = range_2 then 0 else 1)
    done;

    if not (Array.mem (n-1) a) then phase (2*ammount)

  in
  phase 1;
  Array.init n (fun i -> snd indexs.(i))


(* 
Calcula o maior prefixo em comum 

Observando as cadeias representadas pelos indices da suffix tree resultante da função "construct_suffix_array",

Exemplo:
  #CAT$
  $
  AAC#CAT$
  AC#CAT$
  AT$
  C#CAT$
  CAT$
  T$

vemos que é possível verificar a parte comum de duas strings utilizando o seu prefixo.
Desta forma, comparando sempre a string seguinte com a anterior podemos calcular a lista do tamanho dos prefixos 
de cada string em relação à anterior.

Exemplo:
  0 - #CAT$
  0 - $
  0 - AAC#CAT$
  1 - AC#CAT$
  1 - AT$
  0 - C#CAT$
  1 - CAT$
  0 - T$
*)
let calculate_lcp sa = 
  let lcp = Array.make size2 0 in
  let inv = Array.make size2 0 in
  Array.iteri (fun i el -> inv.(el) <- i) sa;

  let len = ref 0 in
  for i = 0 to size2-1 do
    if inv.(i) > 0 then(
      (* Vamos buscar o suffixo anterior *)
      let j = sa.(inv.(i) - 1) in
      
      (* Enquanto houver texto e for igual incrementa *)
      while ((i + !len < size2) && (j + !len < size2) && dna.[i + !len] == dna.[j + !len]) do
        len := !len + 1
      done;
      
      (* Guarda o tamanho do prefixo *)
      lcp.(inv.(i)) <- !len;
      
      if !len > 0 then len := !len - 1)
  done;
  lcp


(*
A função iterate, utiliza a técnica  sliding window para procurar na suffix array o maior prefixo em comum 
entre dois elementos da suffix array de cadeias de dna diferentes.

Esta função recebe três argumentos:
  lcp        - Os maiores prefixos em comum, resultado da função "calculate_lcp";
  owners_map - A array que indica o dono de cada elemento da suffix array.

Exemplo:
 owners_map | lcp | Suffix Array (as cadeias)
   1        | 0   | #CAT$
   2        | 0   | $
   1        | 0   | AAC#CAT$
   1        | 1   | AC#CAT$     
   2        | 1   | AT$
   1        | 0   | C#CAT$
   2        | 1   | CAT$
   2        | 0   | T$

Neste exemplo, iriamos iterar até termos visto elementos das duas cadeias de dna. Neste caso quando víssemos, de cima para baixo, 
a cadeia "AT$" teriamos já visto elementos que pertencem à cadeia dna1 ("AAC#CAT$" e "AC#CAT$") tal como elementos da cadeia dna2 ("AT$").
Como só agora é que vimos duas cadeias diferentes, comparamos o tamanho do prefixo com o já guardado e se for maior atualizamos. Neste
caso iriamos atualizar porque o maior prefixo até então era 0 e acabamos de encontrar um de tamanho 1.

Resetamos o inicio da janela para a última posição visualizada, e continuamos este processo até atingirmos o fim da suffix array.

Nós atingimos o fim da suffix array quando estivermos a tentar aceder a um elemento que provoque o erro "out of bounds", logo tiramos proveito
do try with para tratar desta exceção intencional.
*)
let iterate lcp owners_map = 
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


let sa  = construct_suffix_array dna size2

let lcp = calculate_lcp sa
(* 
Poderiamos ter utilizado uma hashtable, que é dessa foram que estamos a utilizar esta lista,
mas acabámos por optar por uma lista pois acabariamos com uma hashtable com as chaves de 0 a (n-1)
ordenadas.

O mesmo se aplica ao array lcp.
*)
let owners_map = 
  Array.init (String.length dna) (fun i -> if sa.(i) < size1 then 1 else 2)

let _ = Printf.printf "%d\n" (iterate lcp owners_map)

(*
Ideia do Problema

  A nossa resolução do problema B tira partido das suffix arrays para podermos calcular os maiores prefixos em comum.
  A suffix array é uma array que contém todos os sufixos de uma palavra ordenados alfabéticamente. Isto faz com que
  os sufixos que tenham um inicio em comum estejam consecutivos na suffix array.
  Isto permite-nos calcular o maior prefixo em comum de um elemento em relação ao elemento anterior de uma forma muito simples.
  E finalmente, percorrendo a suffix array do inicio até ao fim, guardar o maior prefixo mal termos o conhecimento de 
  termos visto elementos da suffix array que pertençam a cada uma das duas cadeias de DNA.


Estrutura da Explicação

  Cada função tem um cabeçalho onde é explicado o seu funcionamento. E na parte final do programa é explicado a estrutura do programa
  e o exemplo de uma execução, tal como a razão o propósito de cada parte na grande escala da resolução.


Estrutura do Programa

  1 - Ler e tratar o input;

  2 - Construir a suffix array com a função *construct_suffix_array*;

  3 - Calcular e construir a lista dos maiores prefixos em comum. Função *calculate_lcp*;

  4 - Mapear cada elemento da suffix array à cadeia a que pertence.

  5 - Utilizar os maiores prefixos em comum e a lista de donos para encontrar o maior prefixo entre elementos das
duas cadeias de DNA. Função *iterate*


Exemplo de Execução

  Input:

    AAC
    CAC

  Execução:

    1 - Tratar do input

    dna = AAC#CAC$

    2 - Construir o suffix array

    sa = [3; 7; 0; 1; 5; 2; 6; 4]

    3 - Calcular os prefixos de cada elemento

    lcp = [0; 0; 0; 1; 2; 0; 1; 1]

    4 - Guardar o dono de cada elemento da suffix array

    owners_map = [1; 2; 1; 1; 2; 1; 2; 2]

    5 - Utilizar o método sliding window para calcular o maior prefixo commum

    resultado = 2
*)
