let dna1, n = let s = "ABCABC" in s, String.length s
let dna2, m = let s = "ABC" in s, String.length s

let rec iterate i j = match dna1.[i], dna2.[j] with
  | a, c when a <> c -> false
  | _ -> if j >= (m-1) then true else iterate (i+1) (j+1)

let _ =
  try
    let start = (String.index dna1 dna2.[0]) + 1 in
    Printf.printf (if (iterate start 1) then "Yes\n" else "No\n")
  with _ -> Printf.printf "ENo\n"

