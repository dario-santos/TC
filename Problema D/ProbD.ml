(* 
  Referências: 
   - http://www.di.ubi.pt/~desousa/TC/regexp.ml
   - [Slides 62-69] http://www.di.ubi.pt/~desousa/TC/aula_tc3-pp.pdf
   - https://ubipt.sharepoint.com/sites/TC1920/Documentos%20Partilhados/Aula%20TC%20Prática


  16/06/2020

  Autores: 
  37283 - Pedro Moreira
  39973 - Dário Santos
*)

(* 
  O tipo de dados *regexp* representa uma expressão regular,
  Esta estrutura de dados é idêntica à fornecida pelo Professor Doutor Simão Sousa
  no problema C, (http://www.di.ubi.pt/~desousa/TC/regexp.ml).

  V -> Vazio 
  E -> Epsilon
  C -> Um caractere
  U -> União
  P -> Produto/Concatenação
  S -> Estrela de kleen
*)

(** Algoritmo de MacNaughton - Yamada para calcular uma expressão regular a partir de um autómato **)
open List
open Scanf
open Array


(* Definição dos tipos de dados as expressões regulares *)
type regexp =
 | V  
 | E
 | C of char
 | U of regexp * regexp 
 | P of regexp * regexp 
 | S of regexp

(* 
  Como era pedido no enunciado, foi utilizada a função de conversão de uma expressão regular para
  string, utilizando a função presente nos recursos do problema C, (http://www.di.ubi.pt/~desousa/TC/regexp.ml).

  Notas: 
    V deve ser representado com 0
    E deve ser representado com 1
*)
let rec string_of_regexp = function
  | V       -> "0"
  | E       -> "1"
  | C c    -> String.make 1 c    
  | U (f,g) -> "("^(string_of_regexp f)^" + "^(string_of_regexp g)^")"
  | P (f,g) -> "("^(string_of_regexp f)^" . "^(string_of_regexp g)^")"
  | S s     -> (string_of_regexp s)^"*"

(* 
  A função ordenar é responsável por ordenar a expressão regular final, antes de ser realizado o output final.
*)
let ordenar expr =
  (* 
    função utilitária utilizada pela função order, é responsável por ir buscar
    o primeiro carácter de uma expressão regular.

    Exemplo
    r = (((c + d) . d) + a) + d

    get_first r -> a
   *)
  let rec get_first = function 
    | V   -> '0'
    | E   -> '1'
    | C c -> c
    | U (f, g) ->  
      let c1 = get_first f in
      let c2 = get_first g in
      (min c1 c2)
    | P (f, g) -> get_first f
    | S s      -> get_first s
  in
  (* 
    A função order ordena toda a expressão regular.
    A única situação que deve ser modificada é a união em que a ordem
    não modificada a expressão regular.

    O critério de ordenação é:

    0 < 1 < ... < c, em que c é qualquer letra do alfabeto.
  *)
  let rec order = function
    | U (f, g) -> 
      let e1 = order f in
      let e2 = order g in
      let c1 = get_first e1 in
      let c2 = get_first e2 in
      if (min c1 c2) = c1 then U(e1, e2) else U(e2, e1)
    | P (f, g) -> 
      let e1 = order f in
      let e2 = order g in
      P(e1, e2)
    | S s -> S(order s)
    | _ as e -> e (* Não fazer nada em outros casos*)
  in
  order expr


(*
  simplify= função que simplifica "um pouco" a expressão regular
  realisa uma simplificação maior do que a que foi sugerida no enunciado do problema

  Esta função é utilizada durante dos R, e foi fornecida pelo professor no enunciado do problema.
*)  
let rec simplify (a:regexp) = 
 match a with 
 | U (r,s) ->
   let sr = simplify r in
   let ss = simplify s in
   if sr = V then ss
   else if ss = V then sr
   else if ss = sr then sr
   else U (sr,ss) 
 | P (r,s) ->
   let sr = simplify r in
   let ss = simplify s in
   if sr = V then V
   else if ss = V then V
   else if sr = E then ss
     else if ss = E then sr
   else P (sr,ss) 
 | S r -> let sr = simplify r in
   if sr = V || sr = E 
   then E else (
     match sr with
       U (E,rr) | U (rr,E) -> S rr       
       | _ -> S sr
     )
 |  _ -> a


(* 
  Formato de entrada: 
   n - Número de estados do automato
   i - Estado inicial do automato, é deterministico só existe um
   f - Número de estados finais
   finais - linha com f estados separados por um espaço
   m - número de transições
   *m linhas, cada com uma transição*

   Exemplo de uma transição:
   1 a 2

   *De 1 para 2 por a*
*)
let n = Scanf.scanf " %d" (fun a -> a)

let i = Scanf.scanf " %d" (fun a -> a)

let f = Scanf.scanf " %d" (fun a -> a)

let finais =
  let l = ref [] in
  (* Lê f numeros, os estados finais *)
  for i = 0 to f-1 do
    (* Descarta os espaços intermedios, e o \n da linha anterior *)
    Scanf.scanf "%c" (fun _ -> ());
    (* Lê o estado*)
    let tmp = Scanf.scanf "%d" (fun a -> a) in
    (* Actualiza a lista de estados finais*)
    l := tmp::(!l)
  done;
  !l

let m = Scanf.scanf " %d" (fun a -> a)

(*
  Exemplo de uma transição, na entrada:
  i a j

  Pode ser lida como: 
  De i para j por a

  Para este exercício queremos apenas, dependendo do i e do j saber qual o caractere da sua transição,
  isto é o ambiente perfeito para se utilizarem hash tables.

  Utilizamos os valores dos estados como chave e o consumo da sua transição como valor.

  Por exemplo:
  Se tivermos a hashtable, tbl, com as transições:
   (1, 1) = [a]
   (1, 2) = [b; c]
   (3, 2) = [b]

  Durante a execução do algoritmo MacNaughton-Yamada, para R(i, j, k) com k=1, teriamos 
  apenas que aceder o seu valor nesta hashtable:

  R(1, 1, 1) = (Hashtbl.find tbl (1, 1)) + E = E + a
  R(1, 2, 1) = Hashtbl.find tbl (1, 2) = c + b
  R(3, 1 ,1) = Hashtbl.find tbl (3, 1) = V
  R(3, 2, 1) = Hashtbl.find tbl (3, 2) = b
  R(3, 3 ,1) = (Hashtbl.find tbl (3, 3)) + E = E
*)
let transicoes = 
 let t = Hashtbl.create m in
 for i = 0 to m-1 do
  (* Separa a chave do seu valor *)
  let k, v = Scanf.scanf " %d %c %d" (fun a b c-> (a, c), b) in
  (* Pode haver mais do que uma transição para o mesmo estado *)
  try
    let f = Hashtbl.find t k in
    f := !f@[v] 
  with Not_found -> Hashtbl.add t k (ref [v])

 done;
 t



(*
  A hashtable anterior guarda a informação apenas das transições do automato.

  É ainda necessário uma estrutura de suporte para o algoritmo, MacNaughton-Yamada.

  Esta estrutura permite que guardemos os valores intermédios do algoritmo de forma a não serem necessárias
  recalculações.

  Como foi dito para a hashtable ```transicoes```, este é o ambiente ideal para se utilizar uma hashtable.

  Temos as chaves (i, j, k), e os valores R(i, j, k).

  Utilizando o exemplo dado na hashtable transições, iriamos guardar cada um dos R calculados da seguinte forma:
  

  Por exemplo:
  Se tivermos a hashtable, tbl, com as transições:
   (1, 1) = [a]
   (1, 2) = [b; c]
   (3, 2) = [b]

  Durante a execução do algoritmo MacNaughton-Yamada, para R(i, j, k) com k=1, teriamos 
  apenas que aceder o seu valor nesta hashtable:

  Hashtable yamada:
    (3, 2, 1) = C b
    (3, 1 ,1) = V
    (1, 1 ,1) = U(E, C(a)) 
    (1, 2 ,1) = U(C(c), C(b))
*)
let yamada :((int * int * int, regexp) Hashtbl.t) = Hashtbl.create 16

(* 
  A array ```arr``` guarda os valores de R que são necessários calcular, é uma array com n+1 elementos, em que n é o número de estados do automato.

  Cada elemento desta array é uma lista que guarda a informação dum k diferente do algoritmo MacNaughton-Yamada. 
    O elemento arr.(0) guarda os R com k = 1. 
     ...
    O elemento arr.(n) guarda os R com k = n+1. 
  
  Desta forma são apenas calculados os valores de R necessários.

  Inicialmente, inicializamos esta array com os valores de R necessários para calcular a nossa expressão regular.
  Estes valores de R são a união do estado inicial para todos os estados finais.


  Exemplo:
  Se temos o estado inicial: 4
  E os estados finais: 2, 3 e 8

  Iremos inicializar o elemento n+1 da array com o valor:
    [(4, 2); (4, 3); (4, 8)]
*)
let arr = 
  let a = Array.make (n+1) [] in
  (* O inicial para todos os finais*)
  List.iter (fun f -> a.(n) <- (i, f)::a.(n)) finais;
  a

(* 
  A função save_r, é uma das duas funções que compõe a nossa implementação do algoritmo MacNaughton-Yamada.

  Do algoritmo temos que:
    R(i, j, k) = R(i, j, k-1) + R(i, k-1, k-1)R(k-1, k-1, k-1)*R(k-1, j, k-1)

  O cálculo dum R com k+1 requer determinados R com k. Devido a esta recursividade, não percorremos a array ```arr``` do inicio para o fim, mas sim do fim para o incio.
  Pois, por exemplo, para sabermos que R com k=2 precisamos de guardar, precisamos antes de guardar os R com k=3.

  Esta função é responsável por guardar estes R necessários para conseguirmos converter o nosso automato para uma expressão regular.

  Uma nota importante é que quando k=1, podemos aceder a estes valores diretamente no automato. No nosso caso, podemos aceder a estes valores diretamente 
  na hashtable ```transicoes```. Devido a isto, não percorremos toda a array ```arr``` mas apenas de 1 a n. E verificando sempre para não adicionarmos duplicados.
*)
let save_r arr =
  (* De k=n+1 até k=2 *)
  for i = 0 to (n-1) do
    let k = n - i in
    List.iter (fun e -> let a,b = e in
      (* Guarda o k para ficar mais legível *)

      (*
        Cada if corresponde a um R necessário para se calcular R(i, j, k+1) 
          R(i, j, k) + R(i, k, k)R(k, k, k)*R(k, j, k) 
      
        Guardamos apenas os que ainda não foram guardados, pois só precisamos de calcular uma única vez.
      *)
      if not (List.mem (a, b) arr.(k-1)) then arr.(k-1) <- (a, b)::arr.(k-1);
      if not (List.mem (a, k) arr.(k-1)) then arr.(k-1) <- (a, k)::arr.(k-1);
      if not (List.mem (k, k) arr.(k-1)) then arr.(k-1) <- (k, k)::arr.(k-1);
      if not (List.mem (k, b) arr.(k-1)) then arr.(k-1) <- (k, b)::arr.(k-1)
    ) arr.(k)
  done

(* 
  A função calculate_r, é a segunda das duas funções que compõe a nossa implementação do algoritmo MacNaughton-Yamada.

  Esta função é executada após o ```save_r``` e é responsável por calcular os valores de R guardados por essa função.

  Se na função ```save_r``` necessitávamos de começar no k mais alto e ir diminuindo, na função ```calculate_r``` fazemos o processo contrário. Começamos por calcular os R
  com k=1, pois temos acesso direto, depois os k=2, que necessitam dos R com k=1, até k=n este que necessita dos R com k=n-1.

  Como foi exposto anteriormente o cálculo de R é realizado da seguinte forma:
    R(i, j, k+1) = R(i, j, k) + R(i, k, k)R(k, k, k)*R(k, j, k) 

  Excepto, quando k=1, pois neste caso temos acesso direto aos valores de R no próprio automato.

  Como para k=1 temos uma situação diferente, os valores de R não dependem de outros R. Separamos o código responsável por k=1 de todos os outros.

  Em k=1 existem três valores diferentes que R pode tomar.
    1. Se a transição (i, j) existir e i=j, R(i, j, 1) = U(E, Hashtbl.find transicoes (i, j));
    2. Se a transição (i, j) existir, R(i, j, 1) = Hashtbl.find transicoes (i, j);
    3. Se a transição (i, j) não existir, R(i, j, 1) = V.

  Devido ao facto de termos uma hashtable, quando uma transição não existe, é levantada a excecção *Not_found*, quando este for o caso temos apenas que guardar V.
  No caso de a transição existir, temos apenas que verificar se i é igual a j e guardar o valor de acordo com os pontos 1 e 2 descritos acima.


  Na segunda parte da função calculate_r, são calculados os valores de R que dependem de outros R. Nesta parte começamos por calcular k=2 e ir subindo até k=n.
  Desta forma temos sempre os valores de R que necessitamos guardados na hashtable ```yamada```.

  O cálculo de R quando (k+1)>1 é feito da seguinte forma:
    R(i, j, k+1) = R(i, j, k) + R(i, k, k)R(k, k, k)*R(k, j, k)

  Logo só temos que aceder à hashtable ```yamada``` e ir guardando os valores da forma corresponde.

  Existe apenas uma situação em que é necessário algum tratamento especial dos dados, quando existe o vazio.

  Existem três situações que devem ser simplificadas na existiência do vazio:
    1. V + r = r + V = r
    2. r.V = V . r = V 
    3. V* = E

  Estas otimizações, e outras, são realizadas pela função ```simplify```.
*)
let calculate_r arr =
  (* Calcula k=1 *)
  (* 
    Falta o caso em que existe mais do que uma transição
  
  *)

  List.iter (fun e ->
    let v =
      
      (* Transições *)
      let r = 
        try
          let r = ref V in
          List.iter (fun c -> r := U(!r, C c)) !(Hashtbl.find transicoes e);
          !r
        with Not_found -> V 
      in

      if fst(e) = snd(e) then  U(E, r) else r (* Se tem ou não epsilon *)
    in
      
    (* simplifica e guarda *)
    Hashtbl.add yamada (fst(e), snd(e), 1) (simplify v)
  ) arr.(0);

  (* Agora que temos k=1, falta os outros k*)
  for i = 1 to n do
    List.iter (fun e -> 
      (* 
        Guardar R + R . R* + R 
        R(i, j, k+1) = R(i, j, k) + R(i, k, k).R(k, k, k)*.R(k, j, k)
      *)

      (* R(i, j, k) *)
      let r1 = Hashtbl.find yamada (fst(e), snd(e), i) in

      (* R(i, k, k) *)
      let r2 = Hashtbl.find yamada (fst(e), i, i) in

      (* R(k, k, k) *)
      let r3 = S(Hashtbl.find yamada (i, i, i))in

      (* R(k, j, k) *)
      let r4 = Hashtbl.find yamada (i, snd(e), i) in

      (* simplifica e guarda R(i, j, k+1) *)
      Hashtbl.add yamada (fst(e), snd(e), i+1) (simplify (U(r1, P(r2, P(r3, r4)))))
    ) arr.(i)
  done


(*
  A função ```print_regexpr``` realiza as últimas uniões simplificações e ordenação da expressão regular.

  Neste ponto da execução temos calculado todos os R necessários para calcular a expressão regular, falta apenas fazer as uniões finais dos R com k=n+1.

  Se a nossa expressão regular necessitava de R(1, 2, 4) e R(1, 3, 4), falta agora realizar a união: 
    U(R(1, 2, 4), R(1, 3, 4))

  Depois de feitas as uniões finais, a exprssão regular é simplificada uma última vez, e finalmente ordenada.

  É então retornado a expressão regular final que será depois imprimida no ecrã.  
*)
let print_regexpr() =
  let r = ref V in
  List.iter (fun (i,j) ->
    r := U(!r, Hashtbl.find yamada (i,j, (n+1)))
  ) arr.(n);
  (* Ultima simplificação, ordenação e retorno *)
  ordenar (simplify !r)
  

(* 1. Guarda os R *)
let _ = save_r arr
(* 2. Calcula os R*)
let _ = calculate_r arr
(* 3. Output *)
(* vizualização do resultado, simplificado *)
(* equivalente a: let () = print_endline (string_of_regexp (simplify result)) *)
let result = print_regexpr()
let () = result |> simplify |> string_of_regexp |> print_endline
  


(* 
  Exemplo de uma execução:

  Indice: 
    1. Input
    2. Execução do Programa
    3. Ouput

  1. Input
    3
    1 
    2
    1 3
    6
    1 a 1
    1 b 2
    2 a 2
    2 b 3
    2 a 3
    3 b 3

  2. Execução do Programa

    a)Começamos por ler o input

    n = 3
    i = 1
    f = 2
    finais = [3; 1] 
    m = 6
    transicoes (hashtbl) = 
    (
      (1, 1) = [a]
      (1, 2) = [b]
      (2, 2) = [a]
      (2, 3) = [a; b]
      (3, 3) = [b]
    )

    b) Calculamos e guarmos os valores de R que necessitamos para calcular a
    expressão regular.

    arr = [|
      [];
      [];
      [];
      [(1, 3); (1, 1)]
    |]

    c) Utilizando a array arr guardar os valores de R necessários calcular,
    os valores de R irão ficar guardados nas posições correspondentes da array ```arr```,
    R(i, j, 1) irão ficar guardados na primeira posição da array ```arr```, 
    - Função save_r.

    arr = [|
      [(3, 2); (2, 2); (3, 1); (3, 3); (2, 1); (2, 3); (1, 3); (1, 2); (1, 1)];
      [(1, 1); (1, 2); (1, 3); (2, 3); (3, 3); (2, 1); (2, 2); (3, 2); (3, 1)];
      [(3, 1); (3, 3); (1, 3); (1, 1)];
      [(1, 3); (1, 1)]
    |]

    D) Calculamos os valores de R presentes na array ```arr```, e vamos guardando o seu valor
    na hashtable ```yamada``` - Função calculate_r.

    Os valores a baixo são os valores guardados na tabela de hash, ```yamada```, tratados à mão
    para terem uma representação mais agradável e legível. Durante a execução, os valores nesta hashtable
    poderão estar misturados sem qualquer critério de ordenação.

    k = 1 
    ========================================================
    (1, 1, 1) = (1 + a)
    (1, 2, 1) = b
    (1, 3, 1) = 0
    (2, 1, 1) = 0
    (2, 2, 1) = (1 + a)
    (2, 3, 1) = (b + a)
    (3, 1, 1) = 0
    (3, 2, 1) = 0
    (3, 3, 1) = (1 + b)
     
    k = 2 
    ========================================================
    (1, 1, 2) = ((1 + a) + ((1 + a) . (a* . (1 + a))))
    (1, 2, 2) = (b + ((1 + a) . (a* . b)))
    (1, 3, 2) = 0
    (2, 1, 2) = 0
    (2, 2, 2) = (1 + a)
    (2, 3, 2) = (b + a)
    (3, 1, 2) = 0
    (3, 2, 2) = 0
    (3, 3, 2) = (1 + b)

    k = 3 
    ========================================================
    (1, 1, 3) = ((1 + a) + ((1 + a) . (a* . (1 + a))))
    (1, 3, 3) = ((b + ((1 + a) . (a* . b))) . (a* . (b + a)))
    (3, 1, 3) = 0
    (3, 3, 3) = (1 + b)

    k = 4
    ========================================================
    (1, 1, 4) = ((1 + a) + ((1 + a) . (a* . (1 + a))))
    (1, 3, 4) = (((b + ((1 + a) . (a* . b))) . (a* . (b + a))) + (((b + ((1 + a) . (a* . b))) . (a* . (b + a))) . (b* . (1 + b))))

    Alguns casos importantes em cima são:
      - R(2, 3, 1) - Cujo valor é a união de duas transições, uma por b e uma por a.
      - R(2, 2, 2) - que sofreu uma simplificação devido ao facto do segundo elemento da união ser vazio;
      - R(2, 1, 2) - que sofreu uma simplificação devido a ser a união de vazios;
      - R(1, 3, 3) - que sofreu uma simplificação devido ao facto do primeiro elemento da união ser vazio.

  3. Ouput

    a) Fazer a união dos valores na posição n+1 da array ```arr``` n+1 (4), utilizando a tabela de hash
    ```yamada``` para aceder aos seus respectivos valores, a última simplificação e ordenação.

    Elementos em arr.(n+1):
      [(1, 3); (1, 1)]

    É feita a união dos valores de R destes elementos, uma última simplificação e a ordenação necessária:

    (((1 + a) + ((1 + a) . (a* . (1 + a)))) + (((((1 + a) . (a* . b)) + b) . (a* . (a + b))) + (((((1 + a) . (a* . b)) + b) . (a* . (a + b))) . (b* . (1 + b)))))
*)
