(* 
  Referências: 
   - http://www.di.ubi.pt/~desousa/TC/regexp.ml
   - (Slide 62) http://www.di.ubi.pt/~desousa/TC/aula_tc3-pp.pdf
   - https://ubipt.sharepoint.com/sites/TC1920/Documentos%20Partilhados/Aula%20TC%20Prática


  10/06/2020

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
  | C  c    -> String.make 1 c    
  | U (f,g) -> "("^(string_of_regexp f)^" + "^(string_of_regexp g)^")"
  | P (f,g) -> "("^(string_of_regexp f)^" . "^(string_of_regexp g)^")"
  | S s     -> (string_of_regexp s)^"*"

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
   (1, 1) = a
   (1, 2) = b
   (3, 2) = b

  Durante a execução do algoritmo MacNaughton-Yamada, para R(i, j, k) com k=1, teriamos 
  apenas que aceder o seu valor nesta hashtable:

  R(3, 2, 1) = Hashtbl.find tbl (3, 2) = b
  R(3, 1 ,1) = Hashtbl.find tbl (3, 1) = V
  R(1, 1 ,1) = Hashtbl.find tbl (1, 1) = a 
  R(1, 2 ,1) = Hashtbl.find tbl (1, 2) = b
*)
let transicoes = 
 let t = Hashtbl.create m in
 for i = 0 to m-1 do
  (* Separa a chave do seu valor *)
  let k, v = Scanf.scanf " %d %c %d" (fun a b c-> (a, c), b) in
  Hashtbl.add t k v
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
  
  Hashtable yamada:
    (3, 2, 1) = C b
    (3, 1 ,1) = V
    (1, 1 ,1) = C a 
    (1, 2 ,1) = C b
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
*)
let calculate_r arr =
  (* Calcula k=1 *)
  List.iter (fun e ->  
    let v = 
      try
        let c = Hashtbl.find transicoes e in
        if fst(e) = snd(e) then  U(E, C(c)) else C(c) (* Se tem ou não epsilon *)
      with Not_found -> V (* Não existe -> Vazio*)
    in
      
    Hashtbl.add yamada (fst(e), snd(e), 1) v 
  ) arr.(0);

  (* Agora que temos k=1, falta os outros k*)
  for i = 1 to n do
    List.iter (fun e -> 
      (* Guardar R + R . R* + R 
        R(i, j, k+1) = R(i, j, k) + R(i, k, k).R(k, k, k)* . R(k, j, k)
   
        Atenção às otimizações do V (vazio)

          V + r = r + V = r
          r.V = V . r = V
          V* = E
      *)

      (* R(i, j, k) -> Se for V ignoramos r1 *)
      let r1 = Hashtbl.find yamada (fst(e), snd(e), i) in

      (* R(i, k, k) -> Se for V ignoramos r2, r3 e r4*)
      let r2 = Hashtbl.find yamada (fst(e), i, i) in

      (* R(k, k, k) -> Se for V passamos a ter E *)
      let r3 = if(Hashtbl.find yamada (i, i, i)) = V then E else S(Hashtbl.find yamada (i, i, i)) in

      (* R(k, j, k) -> Se for V ignoramos r2, r3 e r4*)
      let r4 = Hashtbl.find yamada (i, snd(e), i) in

      (* Se temos U(V, V) passamos a ter V *)
      let tmp = if r1 = V && (r2 = V || r4 = V) then
        V
       else
          U(r1, P(r2, P(r3, r4)))
       in

      (* Ingora r1 caso r1 seja vazio*)
      let tmp = if r1 = V && (r2 <> V && r4 <> V) then P(r2, P(r3, r4)) else tmp in
      (* Ingora r2, r3 e r4 caso r2 ou r4 seja vazio*)
      let tmp = if r1 <> V && (r2 = V || r4 = V) then r1 else tmp in

      (* Guarda R(i, j, k+1)*)
      Hashtbl.add yamada (fst(e), snd(e), i+1) tmp
    ) arr.(i)
  done


(*
  A função ```print_regexpr``` realiza as últimas uniões e o output final do programa.

  Neste ponto da execução temos calculado todos os R necessários para calcular a expressão regular, falta apenas fazer as uniões finais dos R com k=n+1.

  Se a nossa expressão regular necessitava de R(1, 2, 4) e R(1, 3, 4), falta agora realizar a união: 
    U(R(1, 2, 4), R(1, 3, 4))

  Esta função realiza estas uniões necessárias, tendo sempre em conta a possibilidade de encontrar o vazio.
  Em que nesses casos realiza a simplificação: 
    V + r = r + V = r

  Depois de feitas as uniões, é utilizada a função string_of_regexp, fornecida pelo professor nos recursos do problema C, para realizar o output final do programa.
*)
let print_regexpr() =
  let r = ref V in

  List.iter (fun (i,j) -> 
    let r1 = Hashtbl.find yamada (i,j, (n+1)) in
    (* 
      Simplificação:
        V + r = r + V = r  
    *)
    let tmp = if !r = V then r1 else U(!r, r1) in
    let tmp = if r1 = V then !r else tmp in
     
    r := tmp;

  ) arr.(n);
  Printf.printf "%s\n" (string_of_regexp !r)



(* 1. Guarda os R *)
let _ = save_r arr
(* 2. Calcula os R*)
let _ = calculate_r arr
(* 3. Output *)
let _ = print_regexpr()


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
      (2, 2) = a
      (1, 1) = a
      (1, 2) = b
      (3, 3) = b
      (2, 3) = a
      (2, 3) = b
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
    (2, 3, 1) = a
    (3, 1, 1) = 0
    (3, 2, 1) = 0
    (3, 3, 1) = (1 + b)
     
    k = 2 
    ========================================================
    (1, 1, 2) = ((1 + a) + ((1 + a) . ((1 + a)* . (1 + a))))
    (1, 2, 2) = (b + ((1 + a) . ((1 + a)* . b)))
    (1, 3, 2) = 0
    (2, 1, 2) = 0
    (2, 2, 2) = (1 + a)
    (2, 3, 2) = a
    (3, 1, 2) = 0
    (3, 2, 2) = 0
    (3, 3, 2) = (1 + b)

    k = 3 
    ========================================================
    (1, 1, 3) = ((1 + a) + ((1 + a) . ((1 + a)* . (1 + a))))
    (1, 3, 3) = ((b + ((1 + a) . ((1 + a)* . b))) . ((1 + a)* . a))
    (3, 1, 3) = 0
    (3, 3, 3) = (1 + b)

    k = 4
    ========================================================
    (1, 1, 4) = ((1 + a) + ((1 + a) . ((1 + a)* . (1 + a))))
    (1, 3, 4) = (((b + ((1 + a) . ((1 + a)* . b))) . ((1 + a)* . a)) + (((b + ((1 + a) . ((1 + a)* . b))) . ((1 + a)* . a)) . ((1 + b)* . (1 + b))))

    Alguns casos importantes em cima são:
      - R(2, 2, 2) - que sofreu uma simplificação devido ao facto do segundo elemento da união ser vazio;
      - R(2, 1, 2) - que sofreu uma simplificação devido a ser a união de vazios;
      - R(1, 3, 3) - que sofreu uma simplificação devido ao facto do primeiro elemento da união ser vazio.

  3. Ouput

    a) Fazer a união dos valores na posição n+1 da array ```arr``` n+1 (4), utilizando a tabela de hash
    ```yamada``` para aceder aos seus respectivos valores.

    Elementos em arr.(n+1):
      [(1, 3); (1, 1)]

    União dos seus valores, output:

    (((1 + a) + ((1 + a) . ((1 + a)* . (1 + a)))) + (((b + ((1 + a) . ((1 + a)* . b))) . ((1 + a)* . a)) + (((b + ((1 + a) . ((1 + a)* . b))) . ((1 + a)* . a)) . ((1 + b)* . (1 + b)))))



  (((b + ((1 + a) . ((1 + a)* . b))) + ((b + ((1 + a) . ((1 + a)* . b))) . ((1 + a)* . (1 + a)))) + (((b + ((1 + a) . ((1 + a)* . b))) . ((1 + a)* . b)) + (((b + ((1 + a) . ((1 + a)* . b))) . ((1 + a)* . b)) . ((1 + b)* . (1 + b)))))
  (((b + ((1 + a) . ((1 + a)* . b))) +  (b + ((1 + a) . ((1 + a)* . b))) .  (1 + a)* . (1 + a)) +    ((b +  (1 + a) .  (1 + a)* . b) .    (1 + a)* . b) +   ((b +  (1 + a) .  (1 + a)* . b) .    (1 + a)* . b) .   (1 + b)* . (1 + b))

*)











(* 
  Funções de debug

  As funções a baixo tem apenas teor de depuração, e imprimem o valor de todas as variáveis utilizadas neste programa.
  Caso pretenda visualizar o seu conteúdo deverá descomentar as últimas duas linhas do programa.

  Nota: 
    0 -> vazio;
    1 -> epsilon.
*)
let debug () = 
  Printf.printf "Numero de estados: %d\n" n;
  Printf.printf "Estado Inicial: %d\n" i;
  Printf.printf "Numero de estados finais: %d\n" f;
  Printf.printf "Estados finais:\n";
  List.iter (fun f -> Printf.printf " - %d\n" f) finais;
  Printf.printf "Numero de transicoes: %d\n" m;
  Hashtbl.iter (fun k v -> let a,b = k in Printf.printf "(%d, %d) -> %c\n" a b v) transicoes;
  Printf.printf "Hash table do algoritmo:\n";
  Hashtbl.iter (fun k v -> let i,j,k = k in Printf.printf "R(%d, %d, %d) %s\n" i j k (string_of_regexp v)) yamada

let debug_r arr = 
  Printf.printf "Elementos R\n";
  Array.iteri (fun i r -> 

    Printf.printf "Lista em k=%d\n" (i+1);
    List.iter (fun e -> let i,j = e in Printf.printf " - R(%d, %d)\n" i j) r
  ) arr;
  Hashtbl.iter (fun f v -> let a,b,c = f in Printf.printf "R(%d, %d, %d) = %s\n" a b c (string_of_regexp v)) yamada

(*
  let _ = debug()

  let _ = debug_r arr
*)
