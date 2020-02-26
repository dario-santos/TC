let dna1, n = let s =  "_" ^ read_line() in s, String.length s
let dna2, m = let s =  "_" ^ read_line() in s, String.length s

let matriz = Array.make_matrix n m max_int
let _ = matriz.(0).(0) <- 0

let get i j = let i, j = max i 0, max j 0 in matriz.(i).(j)

let min i j =
  let m = if get i (j-1) <= get (i-1) (j-1) then get i (j-1) else get (i-1) (j-1) in
  if m <= get (i-1) j then m else get (i-1) j

let operation i j = 
  matriz.(i).(j) <- if dna1.[i] = dna2.[j] then get (i-1) (j-1) else (min i j) + 1

let main = function
  | 0, _ -> m | _, 0 -> n
  | _,_ ->  
    Array.iteri (fun i a -> (Array.iteri (fun j _ -> operation i j) a)) matriz;
    matriz.(n - 1).(m - 1)

let _ = Printf.printf "%d\n" (main (n,m))
