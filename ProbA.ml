let dna1, n = let s = "_" ^ read_line() in s, String.length s
let dna2, m = let s = "_" ^ read_line() in s, String.length s

let matriz =
  let arr = Array.make_matrix n m 0 in
  for i = 1 to n - 1 do arr.(i).(0) <- i; done;
  for j = 1 to m - 1 do arr.(0).(j) <- j; done;
  arr

let min i j =
  let a,b,c = matriz.(i).(j-1), matriz.(i-1).(j-1), matriz.(i-1).(j) in
  let m = if a < b then a else b in 
  if m < c then m else c

let operation i a = 
  for j = 1 to m - 1 do
    matriz.(i).(j) <- if a = dna2.[j] then matriz.(i-1).(j-1) else (min i j) + 1;
  done

let main = function
  | 1, _ -> m-1 | _, 1 -> n-1
  | _ ->
    String.iteri (fun i a -> if i > 0 then operation i a) dna1;
    matriz.(n - 1).(m - 1)

let _ = Printf.printf "%d\n" (main (n, m))

(* 
  # Some performance tests:
  
  Both dna1 and dna2 have 10000 elements and are completly different

  Time to iterave the array - 3s

  ## Main function:
  Array.iteri instead of for loop  - 12s
  String.iteri instead of for loop - 12~13s
  for loops                        - 9~10s
  combining for and String.iteri   - 9~10s

  ## Min function:
  if statements                    - 9~10s    
  ocaml min function               - 11~13s
  don't cache the elements         - 10s
  cache the array elements         - 9s~10s
*)

(* 
  # About our implementation

  ## Dynamic Programming

  In a recursive fibonacci function we could draw the tree:

            10
           /  \
          8    9
         / \   /\
        6  7  7  8
         .... 
  We would end calculating the fib(n) for the same n several times. 
  If we store that value we would simple need to access it.
  This is the principle of dynamic programming.

  ## Levensthein Distance

  In our problem, a nayve implementation would fall in the same problem.
  We would be calculating the distance of the same substrings over and over 
  again.

  We use an array to store common substrings so we don't need to be always
  calculating them.

  In other words, each element of the array represents a subproblem.

  Ex.:

  dna1 = hello
  dna2 = bye

      _ H E L L O
    _ 0 1 2 3 4 5
    B 1 1 X
    Y
    E

  Note: the _ represents the empty string.

  The element X represents the sub problem:
    dna1: HE
    dna2: B
    That returns 2 of distance

  References:
  - Programming Challenges The Programming Contest Training Manual (ISBN: 978-0-387-00163-0)
  - https://en.wikipedia.org/wiki/Levenshtein_distance
*)
