let dna1, n = let s =  "_" ^ read_line() in s, String.length s
let dna2, m = let s =  "_" ^ read_line() in s, String.length s

let matriz =
  let arr = Array.make_matrix n m 0 in
  arr.(0).(0) <- 0;
  for i = 0 to m - 1 do arr.(0).(i) <- i; done;
  for i = 0 to n - 1 do arr.(i).(0) <- i; done;
  arr

let debug () = 
  for i = 0 to n - 1 do
      for j = 0 to m - 1 do
        Printf.printf "%d" matriz.(i).(j);
      done;
      Printf.printf "\n";
    done

let min i j =
  let m = if matriz.(i).(j-1) <= matriz.(i-1).(j-1) then matriz.(i).(j-1) else matriz.(i-1).(j-1) in
  if m <= matriz.(i-1).(j) then m else matriz.(i-1).(j)

let operation i j a b =
  if i = 0 || j = 0 then () else
  matriz.(i).(j) <- if a = b then matriz.(i-1).(j-1) else (min i j) + 1


let main = function
  | 1, _ -> m-1 | _, 1 -> n-1
  | _,_ ->
    String.iteri (fun i a -> String.iteri (fun j b -> (operation i j a b)) dna2) dna1;
    matriz.(n - 1).(m - 1)


    (*for i = 1 to n - 1 do
      for j = 1 to m - 1 do
        matriz.(i).(j) <- if dna1.[i] = dna2.[j] then matriz.(i-1).(j-1) else (min i j) + 1;

      done;
    done;
    *)

let _ = Printf.printf "%d\n" (main (n,m))
