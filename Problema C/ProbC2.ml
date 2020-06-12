(*
  C - caractere
  U - União
  P - concatenar
  S - star
*)

type regexp =
 | V  
 | E
 | C of char
 | U of regexp * regexp 
 | P of regexp * regexp 
 | S of regexp

let transicoes = Hashtbl.create 16

let transicoes_eps = Hashtbl.create 16

let estado = ref 1

(* 
  Adiciona transição epsilon de i para f
  i -(E)-> f 
*)
let add_epsilon_trasicion i f =
  (* Se já existirem transições epsilon para este estado, ou seja ele já existe na tabela,
    então só temos que atualizar a lista dos estsados de chegada.

    Se não existir, vai lançar a excepção Not_found e podemos utilizar isto para adicionar um novo elemento.

    Assim não existe shadowing na tabela
  *)
  try
    let l = Hashtbl.find transicoes_eps f in
    l := i::!l
  with Not_found ->
    Hashtbl.add transicoes_eps f (ref [i])

let rec convert_to_nfa = function
  | V       -> assert false
  | E       ->
    let inicio = !estado in
    estado := !estado + 1;

    (* inicio -(E)-> estado *)
    add_epsilon_trasicion inicio !estado
  | C  c    ->
    let anterior = !estado in
    estado := !estado + 1;
    Hashtbl.add transicoes (anterior, c) !estado 
  | U (f,g) ->
    let inicio = !estado in
    estado := !estado + 1;
    
    (* inicio -(Epsilon)> f *)
    add_epsilon_trasicion inicio !estado;

    (* Calcular f: inicio -(epsilon)->  NFA(f) -(epsilon)> final *)
    (* 1. Guardar as transicoes *)
    convert_to_nfa f;
    
    (* Save last from F *)
    let last_state_f = !estado in

    (* Estado inicial de G *)
    estado := !estado + 1;
    
    (* inicio -(Epsilon)> f *)
    add_epsilon_trasicion inicio !estado;

    (* Calcular g: inicio -(epsilon)-> NFA(g) -(epsilon)-> final) *)
    convert_to_nfa g;

    let last_state_g = !estado in

    (* Estado final *)
    estado := !estado + 1;
    
    (* f -(Epsilon)> fim *)
    add_epsilon_trasicion last_state_f !estado;

    (* g -(Epsilon)> fim *)
    add_epsilon_trasicion last_state_g !estado

  | P (f,g) ->
    let inicio = !estado in
    estado := !estado + 1;
    
    (* inicio -(Epsilon)> f *)
    add_epsilon_trasicion inicio !estado;

    (* Calcular f: inicio -(epsilon)->  NFA(f) -(epsilon)> final *)
    (* 1. Guardar as transicoes *)
    convert_to_nfa f;
    
    (* Calcular g: inicio -(epsilon)-> NFA(g) -(epsilon)-> final) *)
    convert_to_nfa g;

    let last_state_g = !estado in

    (* Estado final *)
    estado := !estado + 1;
    
    (* g -(Epsilon)> fim *)
    add_epsilon_trasicion last_state_g !estado


  | S s     ->
    let inicio = !estado in
    estado := !estado + 1;
    let first_state_s = !estado in
    
    (* Calcular s: 
    
        1. inicio        -(E)->  first(NFA(s))
        2. last(NFA(s))  -(E)-> last
        3. first(NFA(s)) -(E)-> last(NFA(s))
        4. inicio        -(E)-> last    
    *)

    (* 1. inicio -(E)->  first(NFA(s)) *)
    add_epsilon_trasicion inicio !estado;

    (* 1. Guardar as transicoes *)
    convert_to_nfa s;

    (* Save last from F *)
    let last_state_s = !estado in

    (* Estado final *)
    estado := !estado + 1;
    
    (* 2. last(NFA(s))  -(E)-> last *)
    add_epsilon_trasicion last_state_s !estado;

    (* 3. first(NFA(s)) -(E)-> last(NFA(s)) *)
    add_epsilon_trasicion last_state_s first_state_s;

    (* 4. inicio -(E)-> last *)
    add_epsilon_trasicion inicio !estado

(*


 *)
let marcar_finais final =
  let visitados = ref [] in
  let finais = ref [!estado] in

  let rec marcar estado = 
    if List.mem estado !visitados then ();

    try(

      List.iter (fun e -> 
        if not (List.mem e !finais) then finais := e::!finais else ()
      ) !(Hashtbl.find transicoes_eps estado);

      visitados := estado::!visitados;

      List.iter (fun e -> marcar e) !(Hashtbl.find transicoes_eps estado))

    with Not_found -> ()
  in
  marcar !estado;
  finais

let debug () = 
  Printf.printf "Initial state: 1\n";
  Printf.printf "Final state:  %d\n" !estado;
  Printf.printf "Transictions: \n";
  Hashtbl.iter (fun k v -> let (i, c) = k in Printf.printf "  %d -(%c)> %d\n" i c v) transicoes;
  Printf.printf "Epsilon Transictions: \n";

  Hashtbl.iter (fun k v ->
    Printf.printf "De estado %d, para: \n" k;

    List.iter (fun e -> Printf.printf " - %d\n" e) !v    
  ) transicoes_eps


(*let _ = Printf.printf "\n\nTest: a + b\n"
let _ = convert_to_nfa (U(C('a'), C('b')))
let _ = debug ()*)



(*let _ = Printf.printf "\n\nTest: a . b\n"
let _ = convert_to_nfa (P(C('a'), C('b')))
let _ = debug ()*)


let _ = Printf.printf "\n\nTest: (a* | b)\n"
let _ = convert_to_nfa (U(S(C('a')), C('b')))

let finais = marcar_finais !estado


let debug_finais () = 
  Printf.printf "Estados Finais:\n";
  List.iter (fun e -> Printf.printf " - %d\n" e) !finais

let _ = debug ()
let _ = debug_finais ()
