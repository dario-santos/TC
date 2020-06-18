type regexp =
 | V  
 | E
 | C of char
 | U of regexp * regexp 
 | P of regexp * regexp 
 | S of regexp

exception Break of bool
(* 
  Referências: 
   - http://www.di.ubi.pt/~desousa/TC/regexp.ml
   - Referência ao Thomson
   - Referência aos slides do professor
   - Referência ao curso de compiladores de stanford

  18/06/2020

  Autores: 
  37283 - Pedro Moreira
  39973 - Dário Santos
*)

(* 
  É utilizada uma excepção para simular a instrução break,
  esta excepção é utilizada durante o algoritmo de aceitação
  para terminarmos a execução do programa.
*)
exception Break of bool

let nfa_of_regexpr expr =
  (*
    A tabela de hash ```transicoes``` guarda as transições de um estado para outro estado através de um caracter.

    No algoritmo de construção de um NFA que utilizamos, construção de Thompson, um estado pode no máximo ter uma transição
    que consome um caractere.

    A chave utilizada nesta tabela é (int, char) e o seu valor é um int. ((int, char), int)
    
    Que de forma mais direta representa:

    ((estado, consumo), estado_seguinte)
  *)
  let transicoes = Hashtbl.create 16 in
  (*
    A tabela de hash ```transicoes_eps``` guarda as transições epsilon de um estado.

    No algoritmo de construção de um NFA que utilizamos, construção de Thompson, um estado pode ter uma ou duas transições epsilon.

    Logo utilizámos o estado como chave e uma lista de estados com valor (int, int list ref).

    A lista de estados representa os estados a um estado pode chegar através de uma transição epsilon.
  *)
  let transicoes_eps = Hashtbl.create 16 in
  (* 
    A variável ```estado``` é utilizada durante a construção do NFA e depois de terminada a construção do NFA
    representa o valor do estado final do NFA.
  *)
  let estado = ref 1 in
  (*
    A função ```add_epsilon_transition``` é uma função utilitária utilizada durante a construção do NFA
    e adiciona uma transição epsilon de i para f:
      i -(E)-> f
  
    Se já existirem transições epsilon para este estado, ou seja, ele já existe na tabela,
    então só temos que atualizar a lista dos estados de chegada.

    Se não existir, vai lançar a excepção Not_found e podemos utilizar isto para adicionar um novo elemento.

    Assim não existe shadowing na tabela.
  *)
  let add_epsilon_trasition i f =
    try
      let l = Hashtbl.find transicoes_eps i in
      l := f::!l
    with Not_found -> Hashtbl.add transicoes_eps i (ref [f])
  in
  (*
    A função ```convert``` converte uma expressão regular para um NFA *com expressões epsilon*,
    utilizando o algoritmo de Thompson.

    O algoritmo de Thompson tem 5 regras de construção:

      Epsilon:

        (1)--E-->((2))
      
      Caractere(c): 
      
        (1)--c-->((2))

      União(f, g):

             +--E-->(f)--E--+
        (1)--+              +-->((2))
             +--E-->(g)--E--+

      Produto(f, g):

        (1)--E-->(f)--E-->(g)--E-->((2))

      Star(s):
                    +--E--+
                    |     v
        (1) --E-->[(2) s (3)]--E--> ((4))
         |                            ^
         +-------------E--------------+


    As transições necessárias em cada estado são detalhadas na sua secção da função ```convert```.
  *)
  let rec convert = function
    | V       -> () (* Ignoramos o vazio *)
    | E       ->
      (* 
        O caso epsilon tem uma transição:
        1. inicio --E--> estado
      *)
      let inicio = !estado in
      estado := !estado + 1;

      (* 1. inicio --E--> estado *)
      add_epsilon_trasition inicio !estado
    | C c     ->
      (* 
        O caso caractere tem uma transição:
        1. inicio --c--> estado
      *)
      let inicio = !estado in
      estado := !estado + 1;
      (*1. inicio --c--> estado *)
      Hashtbl.add transicoes (inicio, c) !estado 
    | U (f, g)->
      (* 
        O caso União tem no mínimo 4 transições:
        1. inicio --E--> inicio de f
        2. inicio --E--> inicio de g
        3. fim de f --E--> fim
        4. fim de g --E--> fim
      *)
      let inicio = !estado in
      estado := !estado + 1;
      
      (* 1. inicio --E--> inicio de f *)
      add_epsilon_trasition inicio !estado;

      (* Converter a subexpressão regular f *)
      convert f;
      
      (* Guarda o valor do último estado de f *)
      let last_state_f = !estado in

      (* Estado inicial de G *)
      estado := !estado + 1;
      
      (* 2. inicio --E--> inicio de g *)
      add_epsilon_trasition inicio !estado;

      (* Converter a subexpressão regular g *)
      convert g;

      (* Guarda o valor do último estado de g *)
      let last_state_g = !estado in

      (* Estado final *)
      estado := !estado + 1;
      
      (* 3. fim de f --E--> fim *)
      add_epsilon_trasition last_state_f !estado;

      (* 4. fim de g --E--> fim *)
      add_epsilon_trasition last_state_g !estado
    | P (f,g) ->
      (* 
        O caso Produto tem no mínimo 4 transições:
        1. inicio --E--> inicio de f
        2. fim de f --E ou c--> inicio de g
        3. fim de g --E--> fim
      *)
      let inicio = !estado in
      estado := !estado + 1;
      
      (* 1. inicio --E--> inicio de f *)
      add_epsilon_trasition inicio !estado;

      (* Converter a subexpressão regular f *)
      convert f;
      
      (*
        Devido à forma como o algoritmo de conversão está implementado quando g for convertido,
        este vai buscar o valor da variável ```estado``` que vai ser o valor do último estado 
        da subexpressão f e adicionar uma transição de f para o início de g.
        Esta transição pode tanto ser com ou sem consumo (ser ou não epsilon).
      *)

      (* Converter a subexpressão regular g *)
      convert g;

      (* Guarda o valor do último estado de g *)
      let last_state_g = !estado in

      (* Estado final *)
      estado := !estado + 1;
      
      (* 3. fim de g --E--> fim *)
      add_epsilon_trasition last_state_g !estado
    | S s     ->
      (*
        O caso Star tem no mínimo 4 transições:
        1. inicio --E--> inicio de s
        2. fim de s --E--> fim
        3. fim de s --E--> inicio de s
        4. inicio --E--> fim
      *)
      let inicio = !estado in
      estado := !estado + 1;
      (* Guarda o primeiro estado de s *)
      let first_state_s = !estado in

      (* 1. inicio --E--> inicio de s *)
      add_epsilon_trasition inicio !estado;

      (* Converte a subsexpressão s *)
      convert s;

      (* Guarda o último estado de s *)
      let last_state_s = !estado in

      (* Estado final *)
      estado := !estado + 1;
      
      (* 2. fim de s --E--> fim *)
      add_epsilon_trasition last_state_s !estado;

      (* 3. fim de s --E--> inicio de s *)
      add_epsilon_trasition last_state_s first_state_s;

      (* 4. inicio --E--> fim *)
      add_epsilon_trasition inicio !estado
  in
  (*
    A função ```calcular_estados_epsilon``` é responsável por calcular os conjuntos de estados epsilon a que um
    estado com transições epsilon consegue atingir através de qualquer número de transições epsilon.
    Esta é uma otimização para não necessitarmos de estar sempre a recalcular estes conjuntos.

    Como é possível existirem ciclos entre estados, foi  utilizada uma lista que guarda 
    os estados já visitados para não serem recalculados.

    Exemplo:
      NFA:
       (1) --E--> (2) --E--> (3) --A--> (4) --E--> ((5))

      Conjuntos:  
       1: {2, 3} 
       2: {3}
       3: {}
       4: {5}
       5: {}
  *)
  let calcular_estados_epsilon transicoes_eps =
    let visitados = ref [] in
    let estados_seguintes_epsilon = Hashtbl.create 16 in

    (* A função ```marcar``` calcula o conjunto de estados seguintes de um estado *)
    let rec marcar estado = 
      if not (List.mem estado !visitados) then
      (
        visitados := estado::!visitados;

        (* 1. Verificar se existe estados epsilon deste estado *)
        let s = try !(Hashtbl.find transicoes_eps (estado)) with Not_found -> [] in
          
        let tmp = ref s in
        (* 2. Calcular seguintes *)
        List.iter (fun e ->
          (* calcula as dependencias *)
          marcar e;
          (* vamos buscar os seguintes dele *)
          let v = try !(Hashtbl.find estados_seguintes_epsilon e) with Not_found -> [] in
          tmp := !tmp@v
        ) s;

        (* 3. Adicionar *)
        Hashtbl.add estados_seguintes_epsilon estado tmp
      )
    in
    Hashtbl.iter (fun k _ -> marcar k) transicoes_eps;
    estados_seguintes_epsilon
  in
  (* Converte a expressão regular para NFA *)
  convert expr;
  (* Calcula os conjuntos de estados a que cada estado com transições epsilon consegue chegar, apenas com transições epsilon *)
  let estados_seguintes_epsilon = calcular_estados_epsilon transicoes_eps in
  (* Transições com consumo, conjuntos de estados atingíveis por epsilon, estado final*)
  transicoes, estados_seguintes_epsilon, !estado

(* 
  A função ```nfa_accept``` recebe um NFA e uma string e verifica se pelo menos uma parte da string é aceite pelo NFA.
*)
let nfa_accept nfa dna =
  (* Extraímos a informação do NFA *)
  let transicoes, estados_seguintes_epsilon, final = nfa in
  (* 
    A função ```normalize``` foi fornecida pelo Professor Doutor Simão Sousa nos recursos do problema D.

    normalize l = l sem duplicados, de forma eficiente ie. linear! *) 
  let normalize l =
    let tbl = Hashtbl.create (List.length l) in
    let f l e = 
      try 
        Hashtbl.find tbl e; l
        with Not_found -> Hashtbl.add tbl e (); e::l
      in
    List.rev (List.fold_left f [] l)
  in
  (* 
    A função ```next_states``` devolve uma lista com os estados a que podemos chegar partindo de um 
    dado estado e consumindo o caractere fornecido.
  *)
  let next_states state c = 
    try [Hashtbl.find transicoes (state, c)] with Not_found -> []
  in
  (* 
    A função ```accept``` é o coração do problema C, é responsável por verificar
    se alguma secção da string fornecida é aceite por um determinado automato.

    Uma palavra é aceite por um automato se ao chegarmos ao fim dessa palavra estivermos num estado final do automato.


    Pseudo código do algortimo de aceitação de um NFA, os estados atuais são inicializados a 1 pois este é o estado inicial:

      estados_atuais = [1]
      estados_epsilon = []
      estados_seguintes = []

      Para cada caractere na palavra w:
        Calcular os estados seguintes através de transições epsilon

        Calcula os estados seguintes através do consumo do caractere atual da palavra w

        Se não existem estados seguintes, termina.
        
        estados_atuais = estados_seguintes
        estados_epsilon = []
        estados_seguintes = []
      Fim

      Se o estado final pode ser atingido através dos esatdos atuais então
        É aceite
      Se não
        Não é aceite
    
    Como apenas necessitamos que parte da palavra seja aceite utilizamos fazemos duas alterações ao algoritmo de aceitação
    de um NFA:

      1. Forçamos o estado 1, o estado inicial, a estar sempre presente no conjunto de estados atuais. Assim adicionamos uma nova execução partindo
         do caractere atual da palavra às já presentes;
      2. Verificamos em todas as iterações se atingimos o estado final. Se sim, terminamos a execução utilizando a exceção Break.
  *)
  let accept dna =
    (* estado 1 é o inicial *)
    let estados_atuais = ref [1] in
    let estados_seguintes = ref [] in
    let estados_epsilon = ref [] in
    (* Se é ou não necessário pesquisar pelo estado final *)
    let research =
      (try 
        (* Consumir toda a cadeia de dna *)
        for i=0 to (String.length dna)-1 do
          (* Força a presença do estado 1 nos atuais *)
          if not (List.mem 1 !estados_atuais) then estados_atuais := 1::!estados_atuais;

          (* Ir buscar a que estados podemos chegar através de epsilon *)
          List.iter (fun s ->
            let v = try !(Hashtbl.find estados_seguintes_epsilon s) with Not_found -> [] in
            estados_epsilon := !estados_epsilon@v
          ) !estados_atuais;

          estados_epsilon := normalize !estados_epsilon;

          (* Se estamos no estado final termina *)
          if List.mem final !estados_atuais then raise (Break false);

          (* Se estamos no estado final termina *)
          if List.mem final !estados_epsilon then raise (Break false);

          (* Calcular os estados seguintes através de dna.(i) *)
          List.iter (fun s -> estados_seguintes := !estados_seguintes@(next_states s dna.[i])) !estados_atuais;

          (* Calcular os estados seguintes através de dna.(i) *)
          List.iter (fun s -> estados_seguintes := !estados_seguintes@(next_states s dna.[i])) !estados_epsilon;

          estados_seguintes := normalize !estados_seguintes;

          (* Guardar os seguintes como atuais e limpar os seguintes *)
          estados_atuais := !estados_seguintes;
          estados_seguintes := [];
          estados_epsilon := [];
        done;
        true
      with Break v -> v) in
    
    if not research then true
    else
    (
      (* Ir buscar a que estados podemos chegar através de epsilon *)
      List.iter (fun s ->
        let v = try !(Hashtbl.find estados_seguintes_epsilon s) with Not_found -> [] in
        estados_epsilon := !estados_epsilon@v
      ) !estados_atuais;

      estados_atuais := !estados_atuais@(!estados_epsilon);

      (* Se chegarmos ao fim de dna e se em estados_atuais houver um estado final -> YES *)
      List.mem final !estados_atuais
    )
  in
  accept dna
    
(* Lê o Input *)
let exp = ignore(read_line()); V
let dna = read_line()
(*NFA : transicoes, estados_seguintes_epsilon, estado final*)
let nfa = nfa_of_regexpr exp

(* Realiza o output do programa *)
let _ = Printf.printf "%s\n" (if nfa_accept nfa dna then "YES" else "NO")
