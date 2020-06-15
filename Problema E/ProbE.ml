(* 
  Referências: 
   - http://hackingoff.com/compilers/predict-first-follow-set
   - http://www.di.ubi.pt/~desousa/TC/PbE.pdf
   - [Slides 49-61] https://www.di.ubi.pt/~desousa/DLPC/aula_dlpc5-pp.pdf


  15/06/2020

  Autores: 
  37283 - Pedro Moreira
  39973 - Dário Santos
*)

(*
  Uma gramática algébrica pode conter três tipos de valores:
   - Epsilon -- Letra epsilon, neste problema ```_```;
   - Terminais -- Letra minusculas que não contém produções;
   - Não terminais -- Letras maiúsculas, que podem produzir algo.

  Devido a isto, foi criado o tipo de dados valor 
  que contém estes três tipos de valores:
   - E -- Epsilon;
   - T -- Terminais;
   - N -- Não terminais.
*)
type value =
  | E
  | T of char
  | N of char
(* 
  A função ```string_of_value```, converte uma variável do 
  tipo ```valor``` para o tipo ```string```.

  É utilizada para realizar o output final.
*)
let string_of_value = function
  | E -> "_"
  | T c | N c -> Char.escaped c

(*
  A função ```merge_lists``` recebe duas listas e concatena as duas
  removendo os elementos duplicados que encontrar.

  Devido à forma como implementámos o algoritmo não existem duplicados numa lista,
  mas pode passar a haver quando se realiza a concatenação de duas listas. 
  Isto acontece porque sempre que adicionamos um elemento a uma lista verificamos 
  por duplicados, para nunca deixar que uma lista fique poluída com duplicados.
*)
let merge_lists l p =
  (* lista temporária inicializada a l *)
  let t = ref l in
  (* Junta os elementos de p que não estão em l *)
  List.iter (fun e -> if not (List.mem e !t) then t := !t@[e]) p;
  !t


(* 
  Formato de entrada: 
   n - Número de produções.
   n linhas, cada uma com uma produção. 

   Exemplo de uma produção:
   S -> a _ c D

  *S produz "a _ c D"*

  Notas:
   - _ -- Epsilon;
   - c -- Terminal;
   - C -- Não terminal.
*)
let n = Scanf.scanf " %d" (fun x -> x)

(*
  Agora que lemos o número de produções, n, falta ler as n produções.
  Como um não terminal pode conter uma ou mais produções, este pareceu-nos
  o ambiente perfeito para se utilizar uma hashtbl, em que utilizamos os
  não terminais como chave e as suas respectivas produções como os seus valores.

  Exemplo:
   Produções:
    S -> a C d
    S -> f
    C -> _
   Tabela:
   'S': [[T('a'); N('C'); T('a')]; [T('f')]]
   'C': [[E]]
   

  Uma produção tem o formato:
  S -> S a b c d

  BNF:  
    <terminal> ::= ['a' - 'z']
    <nao_terminal> ::= ['A' - 'Z']
    <caractere> ::= terminal | nao_terminal
      
    <produção> ::= <nao_terminal> -> <caractere>*

  E termina quando é atingido o '\n'.

  Logo, temos apenas que:
   1. Ler o não terminal;
   2. Ignorar a seta (->);
   3. Ler os caracteres restantes até atingir o '\n' (ou EOF), separando
    os diversos caracteres pelos espaços. 
*)
let productions =
  (* 
    A função ```add_production``` adiciona uma produção à tabela de produções.

    Existem dois casos que podem acontecer quando estamos a adicionar uma produção:
     1. Não existir o não terminal na tabela, logo criamos;
     2. Ou o não terminal já existir, logo atualizamos a sua lista de produções.
  *)
  let add_production tbl nonTerminal production =
    try
      let p = Hashtbl.find tbl nonTerminal in
      p := !p @ [production]
    with Not_found -> Hashtbl.add tbl nonTerminal (ref [production]) in

  (*
    A função ```read_production``` lê uma produção.

    Existem 5 casos diferentes que o nosso lexer pode encontrar:
     - '\n' -- Quebra de linha, a leitura da produção terminou;
     - ' ' -- Ignoramos o espaço e passamos para o próximo caractere;
     - '_' -- Epsilon, guardamos o valor epsilon na produção e continuamos;
     - caractere maiusculo -- Não terminal, guardamos o caractere não terminal na produçãoe e continuamos;
     - caractere minusculo -- Terminais, guardamos o caractere terminal na produção e continuamos.

     Se por alguma razão atingirmos o fim do ficheiro (EOF) terminanos a leitura.
  *)
  let rec read_production r =
    try
      (* Consumimos o próximo caractere do buffer *)
      match (Scanf.scanf "%c" (fun x -> x)) with 
      | '\n'   -> r (* termina a produção *)
      | ' '    -> read_production r (* vamos para o proximo caractere *)
      | '_' -> read_production (r@[E]) (* epsilon *)
      | _ as c when (Char.uppercase_ascii c) = c -> read_production (r@[N(c)]) (* Se uppercase c = c então é não terminal*)
      | _ as c -> read_production (r@[T(c)]) (* terminal *)
    with End_of_file -> r in

  (* A tabela com as produções *)
  let p = Hashtbl.create 16 in
  
  (* 
    Vamos ler n produções
    Com o formato:

      <terminal> ::= ['a' - 'z']
      <nao_terminal> ::= ['A' - 'Z']
      <caractere> ::= terminal | nao_terminal
      
      <produção> ::= <nao_terminal> -> <caractere>*
  *)
  for i = 1 to n do
    (* Lêmos o não terminal *)
    let nonTerminal = Scanf.scanf " %c" (fun x -> x) in
    (* Ignoramos a seta -> *)
    Scanf.scanf " %s" (fun _ -> ());
    (* Lêmos a produção *)
    let production = read_production [] in
    (* Adicionamos a produção lida à tabela *)
    add_production p nonTerminal production
  done;
  p
(* 
  A função ```calcular_null``` é a primeira função das três que constituem este problema.
  Como o cálculo do first depende do null e o follow do first, o cálculo do nulo é feito primeiro.

  Um não terminal é nulo caso uma das suas produções seja nula, ou seja:
    NULL(S) = NULL(a1) v ... NULL(an), sendo a1 .. an são produções de S

  A verificação da nulabilidade de uma produção, NULL(a1), pode ter três situações diferentes:
   1. Se a produção é epsilon                       então NULL(a1) = True;
   2. Se a produção contém um terminal              então NULL(a1) = False;
   3. Se a produção é constituída por não terminais então NULL(a1) = NULL(X1) ^ ... ^ NULL(m).

  Exemplo:
   1. NULL(_) = True
   2. NULL(a _ C D _ b) = False
   3. NULL(A B C) = NULL(A) ^ NULL(B) ^ NULL(C)

  Algo semelhante às produções e recurrente durante esta resolução foi a utilização de hashtables 
  para representar os nossos dados.

  Basicamente o algoritmo que queremos é:

  Para cada não terminal
    Para cada produção desse não terminal
      Verificar se essa produção pode ser nula
    Fim
  Fim

  Existe uma situação especial, a de existirem ciclos nas produções, por exemplo:

  S -> A
  A -> S

  Para resolver estas situações, foi utilizada uma lista que guarda quais as produções já foram visistadas.
  A lista visitados guarda no formato: (Não terminal, produção), as produções já visitadas.
  Exemplo:
    S -> a c
    A -> S

    A produção "S -> a c" já foi visitada. "A -> S" ainda não.

    visitados = [('S', [T('a'); T('c')])]
*)
let calcular_null productions =
  (* Produções visitadas, (Não Terminal, produção) *)
  let visitados = ref [] in
  (* Tabela de nulos, (Não Terminal, true/false) *)
  let nulltbl = Hashtbl.create 16 in
  
  (* Calcula a nulabilidade de um não terminal *)
  let rec null nonTerminal =
    if not (Hashtbl.mem nulltbl nonTerminal) then (
      (* vai buscar as produções à tabela *)
      let v = try !(Hashtbl.find productions nonTerminal) with Not_found -> [] in

      (* Iteramos as produções *)
      List.iter(fun p ->
        (* Se esta produção ainda não foi visitada *)
        if not (List.mem (nonTerminal, p) !visitados) then (
          (* marca como visitada*)
          visitados := (nonTerminal, p)::!visitados;

          (* 
            A variável isNull é utilizada para verificar a nulabilidade desta produção,
            se no fim da produção estiver como falso então a produção não é nula.
          *)
          let isNull = ref true in
          
          List.iter (fun e ->
            match e with
            | E   -> () (* Não fazemos nada *)
            | T c -> isNull := false (* Esta produção não pode ser nula *)
            | N c ->
              (* calcula a nulabilidade do não terminal C *)
              null c;
              (* Se C não é nulo, então esta produção não pode ser nula *)
              try
                if not (Hashtbl.find nulltbl c) then isNull := false
              with Not_found -> isNull := false
          ) p;

          (* Se esta produção é nula guardamos como tal, mas verificamos por duplicações. Não as queremos *)
          if !isNull then(
            Hashtbl.remove nulltbl nonTerminal; 
            Hashtbl.add nulltbl nonTerminal true)
        )
      ) v;

      (* Se não temos uma produção nula marcamos este não terminal como falso *)
      try
        ignore(Hashtbl.find nulltbl nonTerminal)
      with Not_found -> Hashtbl.add nulltbl nonTerminal false
    )
  in
  (* Calculamos a nulabilidade para todos os não terminais *)
  Hashtbl.iter (fun k _ -> null k) productions;
  (* Só queremos a tabela de nulos *)
  nulltbl


(* Tabela de nulos *)
let nulltbl = calcular_null productions

(* 
  A função ```calcular_first``` é a segunda função das três que constituem este problema.
  Esta função é responsável por calcular os conjuntos first de cada não terminal, utilizando
  o predicado NULL para o conseguir.

  O conjunto first de um não terminal é a junção do first de cada uma das suas produções, ou seja:
    FIRST(S) = FIRST(a1) v ... FIRST(an), sendo a1 .. an são produções de S

  O cálculo do conjunto first de uma produção, FIRST(a1), ter quatro situações diferentes:
   1. Se a produção é epsilon                       então FIRST(a1) = [];
   2. Se a produção contém um terminal no inicio    então FIRST(a1) = [c];
   3. Se a produção é constituída por não terminais então FIRST(a1) = FIRST(X1) ^ ... ^ FIRST(Xm), enquanto NULL(Xi) = True.
   
  Exemplo:
   1. FIRST(_) = {}
   2. FIRST(a _ C D _ b) = {a}
   3. FIRST(A B C) = FIRST(A), se NULL(A) = False
   4. FIRST(A B C) = FIRST(A) v FIRST(B), se NULL(A) = True e NULL(B) = False

  Foi utilizada uma tabela de hash para guardar os dados do first, desta forma foi possível fazer a ligação entre o não terminal
  e o seu conjunto first.

  O algoritmo é:

  Para cada não terminal
    Para cada produção desse não terminal
      Enquanto não lermos um terminal ou um não terminal com nulabilidade falsa
        Adiciona o caractere ao conjunto first deste não terminal
    Fim
  Fim

  De forma a ajudar o algoritmo anterior, possuímos uma variável "is_first" que nos vai dizer se
  o caractere atual pode ser o primeiro aparecer (pertencer ao conjunto first deste não terminal) ou não.
  Esta variável vai passar a falso quando for lido um não terminal ou um não terminal cuja nulabilidade seja falsa.

  Existe também uma situação especial, a de existirem ciclos nas produções, por exemplo:

  S -> A
  A -> S

  Para resolver estas situações, foi utilizada uma lista que guarda quais as produções já foram visistadas.
  A lista visitados guarda no formato (Não terminal, produção) as produções já visitadas.
  Exemplo:
    S -> a c
    A -> S
    A produção "S -> a c" já foi visitada. "A -> S" ainda não.

    visitados = [('S', [T('a'); T('c')])]
*)
let calcular_first productions =
  (* Produções visitadas, (Não Terminal, produção) *)
  let visitados = ref [] in
  (* Tabela dos conjuntos first, (Não Terminal, [ ... ]) *)
  let firsttbl = Hashtbl.create 16 in
  
  let rec first nonTerminal =

    let v = try !(Hashtbl.find productions nonTerminal) with Not_found -> [] in

    (* Iterar as produções deste não terminal *)
    List.iter(fun p ->
      (* Se esta produção já foi visitada então ignora *)
      if not (List.mem (nonTerminal, p) !visitados) then 
      (
        (* Marca como visitada *)
        visitados := (nonTerminal, p)::!visitados;

        (* lista temporária de firsts, que será depois adicionado ao conjunto first deste não terminal *)
        let firsts = ref [] in
        (* este caractere por ser o primeiro? *)
        let is_first = ref true in

        (* Iteramos os caracteres da produção *)
        List.iter (fun e ->
          (* Se é poder ser o primeiro então vamos verificar *)
          if !is_first then 
          (
            match e with
            | E   -> () (* caso do epsilon -> {} *)
            | T c -> (* caso de um terminal -> {c} *)
              if !is_first then firsts := merge_lists !firsts [c];
              is_first := false
            | N c -> (*caso de um não terminal -> FIRST(c) *)

              (* Nao esquecer de atualizar o valor do is_first *)
              (try 
                if not (Hashtbl.find nulltbl c) then is_first := false
              with Not_found -> is_first := false);
              
              (* Calcula os first do não terminal c *)
              first c;
              (* Adiciona ao nosso first *)
              let tmp = try !(Hashtbl.find firsttbl c) with Not_found -> [] in
              firsts := merge_lists !firsts tmp
          )
        ) p;

        (* Adiciona os elementos em first a este não terminal *)
        (try
          let f = Hashtbl.find firsttbl nonTerminal in
          f := merge_lists !f !firsts
        with Not_found -> Hashtbl.add firsttbl nonTerminal firsts);
      )
    ) v
  in
  Hashtbl.iter (fun k _ -> first k) productions;
  (* Só queremos a tabela de first *)
  firsttbl

(* tabela de conjuntos first*)
let firsttbl = calcular_first productions

(* 
  A função ```calcular_follow``` é a terceira função das três que constituem este problema.
  Esta função é responsável por calcular o conjunto follow de cada não terminal, utilizando
  o predicado NULL e os conjuntos FIRST para o conseguir.

  Esta função difere um pouco das anteriores pois, até agora, utilizamos as produções de um não terminal
  para calcular algo que lhe dizia respeito. No caso do follow queremos procurar as ocorrências de um 
  não terminal em todas as produções.

  O cálculo do conjunto follow necessita da inserção de uma nova produção.
    S' -> S#
  Como da forma como foi implementado podemos apenas trabalhar com caracteres, representámos o não terminal S' como:
   +
  Ficando com a produção:
   + -> S #


  O conjunto follow de um não terminal é a junção dos conjunto first dos terminais e não terminais que ocorrem a seguir a si,
  e dos conjunto follow caso este não terminal ocorra no fim de uma produção, ou seja:

  Se tivermos as produções:
  S'-> S #
  S -> A C B
  A -> a
  B -> b
  B -> _
  C -> c

  Follow(S) = First(#) -> Na produção S# o caractere que vem depois de S é o #.
  Follow(A) = First(C) -> Na produção ACB o caractere que vem depois de A é o C, e NULL(C) = False.
  Follow(B) = Follow(S) -> Na produção ACB, B é o último caractere da produção logo o que vier depois de S vai ser o que vem depois de B.
  Follow(C) = First(B) v Follow(S) -> Na produção ACB, o caractere que vem depois de C é o B, e como NULL(B) = True verificamos o que vem depois de B
                                      como B é o último caractere então será o Follow de S.


  Foi utilizada uma tabela de hash para guardar os dados do follow, desta forma foi possível fazer a ligação entre o não terminal
  e o seu conjunto follow.
  Para saber os terminais que vem de trás utilizámos uma lista que guarda os não terminais anteriores, lista ativos, por exemplo:
    S -> A c
    Nesta produção, quando lermos o A este será adicionado à lista ativos para que no próximo caractere possamos saber que temos que
    adicionar o caractere ao follow deste não terminal.

  Foi também utilizada uma segunda tabela de hash para guardar as dependencias dos não terminais. Consideramos uma dependencia como sendo os
  conjuntos follow que um não terminal necessita para poder calcular o seu. 
  Seguindo o exemplo a cima dado:
   C depende do S, pois Follow(C) necessita de Follow(S).

  O algoritmo para o cálculo do follow é:

  Para cada não terminal
    Para cada produção desse não terminal
      Para cada caractere nesta produção
        Se lermos um termininal então
          Adiciona o terminal ao follow de todos os ativos
        Se lermos um não terminal então
          Se poder ser nulo então
            Adicionamos aos ativos
          Se não
            Limpamos os ativos e colocamos apenas o acabado de ler
      Fim
      Se chegarmos ao fim da produção e ainda existirem não terminais então
        Adiciona as dependencias.
    Fim
  Fim

  Depois de percorridas todas as produções, podemos então tratar das dependencias existentes.
  Para o cálculo das dependencias é necessário a lista visitados como foi utilizada no cálculo do first.
  Para que não fiquemos presos dentro de ciclos.
*)
let calcular_follow productions =
  (* Tabela de conjuntos follow , (Não Terminal, [ Terminais ]) *)
  let followtbl = Hashtbl.create 16 in
  (* Tabela de dependencias (Não Terminal, [Não Terminais])*)
  let dependencias = Hashtbl.create 16 in

  (* Vamos percorrer todas as produções existentes *)
  Hashtbl.iter (fun k v ->
    (* Todas as produções desse não terminal *)
    List.iter (fun p ->
      
      (* Os não terminais ativos, a que vamos os follow *)
      let ativos = ref [] in

      (* Iterar todos os caracteres da produção *)
      List.iter (fun c -> 
        match c with
        | E ->  ()(* Nada *)
        | T c ->
          (* Para cada não terminal que vem de trás fica com este terminal no seu follow *)
          List.iter (fun n ->
            try
              let f = Hashtbl.find followtbl n in
              f := merge_lists !f [c]
            with Not_found -> Hashtbl.add followtbl n (ref [c])
          ) !ativos;
          (* Limpamos a lista de ativos como é um caractere terminal *)
          ativos := []
        | N c ->
          (* Para cada não terminal que vem de trás fica com o FIRST deste não terminal *)
          let first = (try !(Hashtbl.find firsttbl c) with Not_found -> []) in
          
          List.iter (fun n ->
            try
              let f = Hashtbl.find followtbl n in
              f := merge_lists !f first
            with Not_found -> Hashtbl.add followtbl n (ref first)
          ) !ativos;

          (* Se este terminal pode ser nulo então adiciona-o aos ativos, se não for nulo então limpa a lista *)
          let tmp = try Hashtbl.find nulltbl c with Not_found -> false in
          ativos := if tmp then (merge_lists !ativos [c]) else [c]
      ) p;

      (* Se chegarmos ao fim da produção com caracteres não terminais ainda ativos, então guardamos esta dependencia
        
        Este é o caso em que:
        S -> a A
        A -> c

        follow(A) = follow(S)
      *)
      List.iter (fun n ->
        try
          let f = Hashtbl.find dependencias n in
          f := merge_lists !f [k]
        with Not_found -> Hashtbl.add dependencias n (ref [k])
      ) !ativos;
    ) !v
  ) productions;

  (* 
    Tratar das dependencias.
  *)
  let visitados = ref [] in

  (* Dependencias*)
  let rec follow nonTerminal =
    let v = try !(Hashtbl.find dependencias nonTerminal) with Not_found -> [] in
    
    (* Formato das produções: 'A' : ['S'; 'B'] *)
    List.iter (fun c ->
      (* Apenas os não visitados *)
      if not (List.mem (nonTerminal, c) !visitados) then
      (
        visitados := (nonTerminal, c)::(!visitados);

        (* Tratar das dependencias de C primeiro *) 
        follow c;

        (* Folllow do não terminal C *)  
        let l = try !(Hashtbl.find followtbl c) with Not_found -> [] in
        (* Adiciona Follow(c) ao Follow(nonTerminal) *)
        try
          let p = (Hashtbl.find followtbl nonTerminal) in
          p := merge_lists !p l
        with Not_found -> Hashtbl.add followtbl nonTerminal (ref l)
      ) 
    ) v
  in
  (* Tratamos de todas as dependencias *)
  Hashtbl.iter (fun k _ -> follow k) dependencias;
  followtbl


(* Adicionar a produção  + -> S# *)
let _ = Hashtbl.add productions '+' (ref [[N('S'); T('#')]])
(* Só queremos a tabela de nulos *)
let followtbl = calcular_follow productions

(* 
  A função ```print_tables``` é a função responsável pelo output final do programa.
  
  Este problema difere dos restantes pois tem um output mais composto,
  não apenas um int ou uma string mas conjuntos que devem respeitar determinadas regras.

  A ordem deve ser:
   1. NULL;
   2. FIRST;
   3. FOLLOW.

  A ordem dos não terminais deve ser alfabética, à excepção do não terminal S,
  que deverá ser o primeiro.

  Os conjuntos (first e follow) devem estar ordenados alfabéticamente, e no caso de
  não existirem elementos não será mostrado nada depois do =.

  Ou seja, um exemplo de um output:

  NULL(S) = True
  NULL(A) = False
  FIRST(S) = s c d
  FIRST(A) =
  FOLLOW(S) = #
  FIRST(A) = #
  
  Começamos por criar uma lista com os não terminais existentes, e ordenamo-la alfabéticamente.

  Depois utilizando a ordem desta lista imprimimos o predicado NULL de cada não terminal.

  Continuando a utilizar a ordem desta lista lêmos o conjunto first correspondente e ordenamo-lo para depois o imprimir.

  E repetimos o que foi feito para o fist, mas para o follow, utilizando a ordem da lista lêmos 
  o conjunto follow correspondente e ordenamo-lo para depois o imprimir.
*)
let print_tables () =
  (* Ordem alfabetica *)
  let order = ref [] in
  (* Ignoramos os não terminais + e S, + não deve ser mostrado, S é adicionado no topo *)
  Hashtbl.iter (fun k _ -> if k <> 'S' && k <> '+' then order := !order@[k]) productions;
  (* Ordena alfabeticamente *)
  order := List.sort compare !order;
  (* Adiciona S no topo *)
  order := 'S'::(!order);

  (* Output da tabela NULL *)
  List.iter (fun k ->
    (* vai buscar o valor deste não terminal *)
    let tmp = try (Hashtbl.find nulltbl k) with Not_found -> false in
    let out = if tmp then "True" else "False" in

    Printf.printf "NULL(%c) = %s\n" k out 
  ) !order;

  (* Output da tabela FIRST *)
  List.iter (fun k ->
    (* vai buscar o conjunto first deste não terminal *)
    let tmp = try !(Hashtbl.find firsttbl k) with Not_found -> [] in
    (* ordenamos alfabeticamente *)
    let firsts = List.sort compare tmp in

    Printf.printf "FIRST(%c) =" k;
    List.iter (fun e -> Printf.printf " %c" e) firsts;
    Printf.printf "\n" 
  ) !order;

  (* Output da tabela FOLLOW *)
  List.iter (fun k ->
    (* vai buscar o conjunto follow deste não terminal*)
    let tmp = try !(Hashtbl.find followtbl k) with Not_found -> [] in
    (* ordena alfabeticamente *)
    let follow = List.sort compare tmp in

    Printf.printf "FOLLOW(%c) =" k;
    List.iter (fun e -> Printf.printf " %c" e) follow;
    Printf.printf "\n" 
  ) !order

(* Realiza o output *)
let _ = print_tables ()


(* 
  Exemplo de uma execução:

  Indice: 
    1. Input
    2. Execução do Programa
    3. Ouput

  1. Input
    4
    S -> A
    A -> _
    A -> S
    S -> a

  2. Execução do Programa

    a) Começamos por ler o input

    n = 4

    produções (hashtbl) = 
    (
      S = [[N(A)]; [T(a)]]
      A = [[N(S)]; [E]]
    )

    b) Calculamos o predicado NULL para cada não terminal, utilizando a função calcular_null.
    Isto irá retornar a tabela nulltbl com o nulo de cada não terminal.

    nulltbl(
      S : true
      A : true
    )

    c) Utilizando a tabela nulltbl, vamos calcular o conjunto FIRST. Utilizando a função calcular_first.
    Isto irá retornar a tabela firsttbl com os conjunto first de cada não terminal.

    firsttbl(
      S : [a]
      A : [a]
    )


    d) Utilizando as tabelas nulltbl e firsttbl, vamos calcular o conjunto FOLLOW. Utilizando a função calcular_follow.
    Isto irá retornar a tabela followtbl com os conjunto follow de cada não terminal.
    
    Começamos por adicionar a produção à tabela de produções:
     + -> S #

    E cálculamos o follow e guardamos as dependencias existentes.

    followtbl(
      S : [#]
      A : []
    )
    
    dependencias(
      S : [A]
      A : [S]
    )

    Tratamos das dependencias e terminamos o cálculo dos conjuntos follow

    followtbl(
      S : [#]
      A : [#]
    )

  3. Ouput

    a) Iteramos a tabela de produções para guardar os não terminais, para que possamos os ordernar alfabeticamente, 
    à excepção do S que será posterioremente adicionado ao topo da lista de ordem de print e do + que deve ser ignorado.

    lista com a ordem:
    order = [S, A]

    b) Fazemos o output do NULL
      NULL(S) = True
      NULL(A) = True

    c) Fazemos o output do FIRST
      FIRST(S) = a
      FIRST(A) = a
  
    d) Fazemos o output do FOLLOW
      FOLLOW(S) = #
      FOLLOW(A) = #
*)
