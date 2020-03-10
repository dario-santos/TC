let dna1, n = let s = "_" ^ read_line() in s, String.length s
let dna2, m = let s = "_" ^ read_line() in s, String.length s

let matriz =
  let arr = Array.make_matrix 2 n 0 in
  for i = 0 to n-1 do arr.(0).(i) <- i done;
  arr

let min arr1 arr2 j =
  let a,b,c = arr2.(j), arr1.(j - 1), arr2.(j - 1) in
  let m = if a < b then a else b in 
  if m < c then m else c

let operation i c =
  let arr1, arr2 = matriz.(i mod 2), matriz.((i-1) mod 2) in
  for j = 0 to n-1 do
    if j = 0 then arr1.(j) <- i
    else if c = dna1.[j] then arr1.(j) <- arr2.(j - 1)
    else arr1.(j) <- (min arr1 arr2 j) + 1
  done

let main = function
  | 1, _ -> m-1 | _, 1 -> n-1
  | _ -> 
    String.iteri (fun i c -> if i > 0 then operation i c) dna2;
    matriz.((m-1) mod 2).(n-1)

let _ = Printf.printf "%d\n" (main (n, m))

(* 
  Teste de stress (10000 * 10000)
  Implementação 1: 4~5s
  Implementação 2: 8~9s
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
            ... 

  For a given n we would end calculating the fib(n) several times. 
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

  If we iterate the array we would get the final result in the 
  position i: n-1, j: m-1.

  References:
  - Programming Challenges The Programming Contest Training Manual (ISBN: 978-0-387-00163-0)
  - https://en.wikipedia.org/wiki/Levenshtein_distance
*)
