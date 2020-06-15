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
  tipo ```valor``` e converte para o tipo ```string```.

  É utilizada para realizar o output final.
*)
let string_of_value = function
  | E -> "_"
  | T c | N c -> Char.escaped c

(*
  A função ```merge_lists``` recebe duas listas e realiza concatena as duas
  removendo os elementos duplicados que encontrar.

  Devido à forma como implementámos o algoritmo não existem duplicados numa lista,
  mas pode haver quando se juntam as duas. Isto acontece porque sempre que adicionamos um elemento
  a uma lista verificamos por duplicados, para nunca deixar que uma lista fique poluída com duplicados.
*)
let merge_lists l p =
  (* lista temporária inicializada a l *)
  let t = ref l in
  (* Junta os elementos de p que não estão em l *)
  List.iter (fun e -> if not (List.mem e !t) then t := !t@[e]) p;
  !t


(* 
  Formato de entrada: 
   n - Número de produções
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
  Como um não terminal pode conter uma ou mais produções este pareceu-nos
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

  Logo, o nosso parser tem apenas que:
   1. Ler o não terminal;
   2. Ignorar a seta;
   3. Ler os caracteres restantes até atingir o '\n' (ou EOF), separando
    os diversos caracteres pelos espaços. 
*)
let productions =
  (* 
    A função ```add_production``` adiciona uma produção à tabela de produções.

    Existem dois casos que podem acontecer quando estamos a adicionar uma produção:
     1. Não existir o não terminal na tabela, logo criamos;
     2. O não terminal já existir, logo atualizamos a sua lista de produções.
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
     - '_' -- Epsilon, guardamos o valor epsilon na produção;
     - caractere maiusculo -- Não terminal, guardamos o caractere não terminal na produção;
     - caractere minusculo -- Terminais, guardamos o caractere terminal na produção.

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
   3. Se a produção é constituída por naõ terminais então NULL(a1) = NULL(X1) ^ ... ^ NULL(m).

  Exemplo:
   1. NULL(_) = True
   2. NULL(a _ C D _ b) = False
   3. NULL(A B C) = NULL(A) ^ NULL(B) ^ NULL(C) ^ NULL(D) 

  Algo semelhante às produções e recurrente durante esta resolução foi a utilização de hashtables 
  para representar os nossos dados.

  Basicamente o que queremos é:

  Para cada não terminal
   Para cada produção desse não terminal
    Verificar se essa produção pode ser nula
   Fim
   Se essa produção pode ser nula então este ter
  Fim

  Existe uma situação especial que a o caso de existirem ciclos nas produções, por exemplo:

  S -> A
  A -> S

  Para resolver estas situações, foi utilizada uma lista que guarda quais as produções já foram visistadas.
  A lista visitados guarda no formato (Não terminal, produção).
  Exemplo:
    A produção "S -> a c" já foi visitada. "A -> S" ainda não.
    S -> a c
    A -> S

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
   3. Se a produção é constituída por naõ terminais então NULL(a1) = NULL(X1) ^ ... ^ NULL(m).

*)
let calcular_first productions =
  (* Produções visitadas, (Não Terminal, produção) *)
  let visitados = ref [] in
  (* Tabela de nulos, (Não Terminal, true/false) *)
  let firsttbl = Hashtbl.create 16 in
  
  let rec first nonTerminal =

    let v = try !(Hashtbl.find productions nonTerminal) with Not_found -> [] in

    (* Iterar as produções deste não terminal *)
    List.iter(fun p ->
      (* Se esta produção já foi avaliada então ignora *)
      if not (List.mem (nonTerminal, p) !visitados) then 
      (
        (* Marca como visitada *)
        visitados := (nonTerminal, p)::!visitados;

        let firsts = ref [] in
        let is_first = ref true in

        List.iter (fun e ->
          (* Se é o primeiro então vamos verificar *)
          if !is_first then 
          (
            match e with
            | E   -> () (* Do nothing *)
            | T c ->
              if !is_first then firsts := merge_lists !firsts [c];
              is_first := false
            | N c ->
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

        (* Guarda os first a este não terminal *)
        (try
          let f = Hashtbl.find firsttbl nonTerminal in
          f := merge_lists !f !firsts
        with Not_found -> Hashtbl.add firsttbl nonTerminal firsts);
      )
    ) v
  in
  Hashtbl.iter (fun k _ -> first k) productions;
  (* Só queremos a tabela de nulos *)
  firsttbl

let firsttbl = calcular_first productions

let calcular_follow productions =
  (* Tabela de nulos, (Não Terminal, true/false) *)
  let followtbl = Hashtbl.create 16 in
  let dependencias = Hashtbl.create 16 in

  (* Vamos percorrer todas as produções existentes *)
  Hashtbl.iter (fun k v ->
    List.iter (fun p ->
      (* Os não terminais que a quem estamos a adicionar os follow *)
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

    'S' -> [['a'; 's'] ; ['a']; ['c']]
  *)
  let visitados = ref [] in

  (* Dependencias*)
  let rec follow nonTerminal =
    let v = try !(Hashtbl.find dependencias nonTerminal) with Not_found -> [] in
    
    (* 'A' : ['S'; 'B'] *)
    List.iter (fun c ->
      if not (List.mem (nonTerminal, c) !visitados) then
      (
        visitados := (nonTerminal, c)::(!visitados);

        (* Tratar das dependencias de C primeiro *) 
        follow c;

        (* Folllow do não terminal C *)  
        let l = try !(Hashtbl.find followtbl c) with Not_found -> [] in
        
        try
          let p = (Hashtbl.find followtbl nonTerminal) in
          p := merge_lists !p l
        with Not_found -> Hashtbl.add followtbl nonTerminal (ref l)
      ) 
    ) v
  in
  Hashtbl.iter (fun k _ -> follow k) dependencias;
  followtbl

(* Só queremos a tabela de nulos *)

(* Adicionar a produção  + -> S# *)
let _ = Hashtbl.add productions '+' (ref [[N('S'); T('#')]])
let followtbl = calcular_follow productions


let print_tables () =
  (* Ordem alfabetica *)
  let order = ref [] in
  Hashtbl.iter (fun k _ -> if k <> 'S' && k <> '+' then order := !order@[k]) productions;
  order := List.sort compare !order;
  (* Adiciona S no topo*)
  order := 'S'::(!order);

  (* Print null table *)
  List.iter (fun k ->
    let tmp = try (Hashtbl.find nulltbl k) with Not_found -> false in
    let out = if tmp then "True" else "False" in

    Printf.printf "NULL(%c) = %s\n" k out 
  ) !order;

  (* Print first table *)
  List.iter (fun k ->
    let tmp = try !(Hashtbl.find firsttbl k) with Not_found -> [] in
    let firsts = List.sort compare tmp in

    Printf.printf "FIRST(%c) =" k;
    List.iter (fun e -> Printf.printf " %c" e) firsts;
    Printf.printf "\n" 
  ) !order;

  (* Print follow table *)
  List.iter (fun k -> 
    let tmp = try !(Hashtbl.find followtbl k) with Not_found -> [] in
    let follow = List.sort compare tmp in

    Printf.printf "FOLLOW(%c) =" k;
    List.iter (fun e -> Printf.printf " %c" e) follow;
    Printf.printf "\n" 
  ) !order

let _ = print_tables ()
