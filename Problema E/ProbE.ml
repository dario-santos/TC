(* 
  E: Epsilon 
  T: Terminal
  N: Não Terminal 
*)
type value =
 | E
 | T of char
 | N of char

let string_of_value = function
  | E -> "_"
  | T c | N c -> Char.escaped c

(* 
  Input 

  Epsilon: _
  Terminais: minusculos
  Não Terminais: maiusculos

  S é sempre a produção inicial

  Uma produção tem o formato:

  S -> a

  n -> número de produções
  
  m linhas cada uma com uma produção da forma:
  S -> B D e
*)

(* Número de produções *)

(* 2 - Ler as produções *)
(* S -> S a b c d*)
let n = Scanf.scanf " %d" (fun x -> x)

let productions =
  let p = Hashtbl.create 16 in
    
  let add_production tbl nonTerminal production =
    try
      let p = Hashtbl.find tbl nonTerminal in
      p := !p @ [production]
    with Not_found -> Hashtbl.add tbl nonTerminal (ref [production]) in

  let rec read_production r =
    try
      match (Scanf.scanf "%c" (fun x -> x)) with 
      | '\n'   -> r (* termina a produção *)
      | ' '    -> read_production r (* vamos para o proximo caractere *)
      | '_' -> read_production (r@[E]) (* epsilon *)
      | _ as c when (Char.uppercase_ascii c) = c -> read_production (r@[N(c)]) (* Se uppercase c = c então é não terminal*)
      | _ as c -> read_production (r@[T(c)]) (* terminal *)
    with End_of_file -> r in

  for i = 1 to n do
    let nonTerminal = Scanf.scanf " %c" (fun x -> x) in
    Scanf.scanf " %s" (fun _ -> ());
    let production = read_production [] in
    add_production p nonTerminal production
  done;
  p

let calcular_null productions =
  (* Produções visitadas, (Não Terminal, produção) *)
  let visitados = ref [] in
  (* Tabela de nulos, (Não Terminal, true/false) *)
  let nulltbl = Hashtbl.create 16 in
  
  let rec null nonTerminal =
    if not (Hashtbl.mem nulltbl nonTerminal) then
    (

      let v = try !(Hashtbl.find productions nonTerminal) with Not_found -> [] in

      List.iter(fun p -> 
        if not (List.mem (nonTerminal, p) !visitados) then 
        (
          visitados := (nonTerminal, p)::!visitados;

          let isNull = ref true in
          
          List.iter (fun e ->
            match e with
            | E   -> () (* Do nothing*)
            | T c -> isNull := false
            | N c ->
              null c;
              (* Só atualizamos para falso *)
              try
                if not (Hashtbl.find nulltbl c) then isNull := false
              with Not_found -> isNull := false
          ) p;

          if !isNull then(
            Hashtbl.remove nulltbl nonTerminal; 
            Hashtbl.add nulltbl nonTerminal true)
        )
      ) v;

      (* Se não foi adicionado então colocamos a falso *)
      try
        ignore(Hashtbl.find nulltbl nonTerminal)
      with Not_found -> Hashtbl.add nulltbl nonTerminal false
    )
  in
  Hashtbl.iter (fun k _ -> null k) productions;
  (* Só queremos a tabela de nulos *)
  nulltbl

let nulltbl = calcular_null productions


(* Junta sem duplicados *)
let merge_lists l p =
  let t = ref l in
  List.iter (fun e -> if not (List.mem e !t) then t := !t@[e]) p;
  !t

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
