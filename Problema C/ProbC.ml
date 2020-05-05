(* 1 - Ler a palavra *)
let word = Scanf.scanf " %s" (fun a -> a)

(* 2 - Ler as produções *)
(* S -> S a b c d*)
let n = Scanf.scanf " %d" (fun x -> x)

let rules =
  let rec readAllRules n rules =
    if n > 0 then
      let s = Scanf.scanf " %c" (fun x -> x) in
      let _ = Scanf.scanf " %s" (fun x -> x) in
      let r = [s] in
      let rules = rules @ [readRule r] in
      readAllRules (n-1) rules
    else
      rules
  and readRule r =
    try
      let s = Scanf.scanf "%c" (fun x -> x) in
      if s <> '\n' then
        if s <> ' ' then
          let r = r @ [s] in
          readRule r
        else
          readRule r
      else
        r
    with End_of_file -> r
  in
  readAllRules n []


(* print_list *)
let rec print_list lst =
  match lst with
  | [] -> Printf.printf "\n"
  | [el] ->
    let () = Printf.printf "%c" el in
    print_list []
  | el :: tl ->
    let () = Printf.printf "%c " el in
    print_list tl


let _ = List.iter print_list rules

(* 3 - Correr o algoritmo*)