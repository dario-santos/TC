let dna1, n = let s = read_line() in s, String.length s
let dna2, m = let s = read_line() in s, String.length s
let matriz = Array.make_matrix 2 n 0
let min = ref 0

let iterate i c =
  let arr = matriz.(i mod 2) in
  for j = 0 to n-1 do
    if c = dna1.[j] then
      (arr.(j) <- (try matriz.((i-1) mod 2).(j-1) + 1 with _ -> 1);
      if arr.(j) > !min then min := arr.(j))
    else 
      arr.(j) <- 0;
  done

let _ =
  (match (n,m) with
  | 0, _ | _, 0 -> ()
  | _ when String.equal dna1 dna2 -> min := n
  | _ -> String.iteri (fun i c -> iterate i c) dna2);
  Printf.printf "%d\n" !min
