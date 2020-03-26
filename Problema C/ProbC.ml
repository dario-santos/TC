let print_production prod = 
  Printf.printf "%s -> %s\n" (fst prod) (snd prod)

(*
  S -> a
  S -> Sa
*)
let l = [
  ("S", "a" ); 
  ("S", "Sa")
]

let debug l = List.iter (fun prod -> print_production prod) l

(* ------------------------------------------------ *)

(* S -> S F *)

let word, n = let s = read_line() in s, String.length s

let productions =
  let sz = Scanf.scanf " %d" (fun a -> a) in
  let p = Array.make sz ["",""] in 

  let parent = Scanf.scanf " %s %s" (fun a _ -> a) in
  let children = read_line() in
  let _ = Printf.printf "%s -> %s\n" parent children in
  ()
  (*for i = 0 to sz-1 do
    p.(i) <- let s = read_line() in [Char.escaped s.[0], List.nth (String.split_on_char '>' s) 1]
  done;
  p*)

 (*Array.iter debug productions*) 