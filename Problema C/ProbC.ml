type production = 
| Terminal of string
| Prod of string * production

let rec print_production = function
| Terminal t -> Printf.printf "%s\n" t;
| Prod(s, e) -> Printf.printf "%s -> " s; print_production e

let _ = print_production Prod("S", (Terminal "a"))