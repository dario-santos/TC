let word = Scanf.scanf " %s" (fun x -> x)

let n = Scanf.scanf " %d" (fun x -> x)

(* Rules *)
let (rules:(char list) list) =
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

let size = String.length word

let (matrix:((char list) array) array) = Array.make_matrix (size + 1) (size) ['0']
let () =
  for i = 0 to size do
    for j = 0 to (size - 1) do
      if i + j <= size then
        matrix.(i).(j) <- []
    done
  done
let () =
  for j = 0 to (size - 1) do
    matrix.(0).(j) <- [(String.get word j)]
  done

(* First step *)
let () =
  let rec start matrix rules =
    for i = 0 to (size - 1) do
      matrix.(1).(i) <- findWhichDeduces (List.hd (matrix.(0).(i))) rules []
    done
  and findWhichDeduces c rules result =
    match rules with
    | [] -> result
    | el :: tl ->
      if List.mem c el then
        let result = result @ [List.hd el] in
        findWhichDeduces c tl result
      else
        findWhichDeduces c tl result
  in
  start matrix rules

(* Algorithm *)
let () =
  let rec cyk matrix rules =
    for i = 2 to size do
      for j = 0 to size - 1 do
        if i + j <= size then
          for k = 1 to i - 1 do
            let product = getCartesianProduct matrix.(k).(j) matrix.(i - k).(j + k) [] in
            let (bool, rule) = inGrammar product rules false [] in
            if bool then
              matrix.(i).(j) <- updateMatrix matrix.(i).(j) rule
          done
      done
    done
  and getCartesianProduct a b result =
    match a with
    | [] -> result
    | el :: tl ->
      let result = combine el b result in
      getCartesianProduct tl b result
  and combine a b result =
    match b with
    | [] -> result
    | el :: tl ->
      let result = result @ [[a; el]] in
      combine a tl result
  and inGrammar product rules bool rule =
    match product with
    | [] -> (bool, rule)
    | el :: tl ->
      let (bool, rule) = checkRules el rules bool rule in
      inGrammar tl rules bool rule
  and checkRules element rules bool rule =
    match rules with
    | [] -> (bool, rule)
    | el :: tl ->
      let r = List.hd el in
      let g = List.tl el in
      if element = g then
        let bool = true in
        if not (List.mem r rule) then
          let rule = rule @ [r] in
          checkRules element tl bool rule
        else
          checkRules element tl bool rule
      else
        checkRules element tl bool rule
  and updateMatrix current candidate =
    match candidate with
    | [] ->
      let current = List.sort (fun x y -> if x > y then 1 else -1) current in
      current
    | el :: tl ->
      if not (List.mem el current) then
        let current = current @ [el] in
        updateMatrix current tl
      else
        updateMatrix current tl
  in
  cyk matrix rules

(* Number where doesnt recognize *)
let findLine matrix =
  let n = ref size in
  let flag = ref true in
  let () =
    for i = 1 to size do
      let () = flag := true in
      let () =
        for j = 0 to (size - 1) do
          if i + j <= size then
            if matrix.(i).(j) <> [] then
              flag := false
        done
      in
      if !flag && i < !n then
        n := i
    done
  in
  (size + 2) - !n

(* Print solution *)
let () =
  if List.mem 'S' matrix.(size).(0) then
    let () = Printf.printf "YES\n" in
    print_list matrix.(size).(0)
  else
    let () = Printf.printf "NO\n" in
    let num = findLine matrix in
    Printf.printf "%d\n" num