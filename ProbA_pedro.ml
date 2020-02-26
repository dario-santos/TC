let dna1 = read_line()
let dna2 = read_line()
(* Retorna o min entre 3 valores *)
let min a b c =
  if a <= b && a <= c then a else
    if b <= a && b <= c then b else c

(* Preenche a matriz *)
let preenche dna1 dna2 =
  let j = String.length dna1 in
  let i = String.length dna2 in
  let matriz = Bigarray.Array2.create Bigarray.int Bigarray.c_layout (j + 1) (i + 1) in
  for i = 0 to String.length dna1 do
    for j = 0 to String.length dna2 do
      if i == 0 && j == 0 then matriz.{i,j} <- 0 else
        if i == 0 && j != 0 then matriz.{i,j} <- j else
          if i != 0 && j == 0 then matriz.{i,j} <- i else
            if (Char.compare (String.get dna1 (i-1)) (String.get dna2 (j-1)) == 0) then matriz.{i,j} <- matriz.{i-1, j-1} else
              matriz.{i,j} <- ((min matriz.{i,j-1} matriz.{i-1,j} matriz.{i-1, j-1}) + 1) 
    done;
  done;
  matriz.{String.length dna1, String.length dna2}

  let mooshakA dna1 dna2 =
  (* Verifica se existe uma vazia *)
  if String.length dna1 == 0 then String.length dna2 else
    if String.length dna2 == 0 then String.length dna1 else (preenche dna1 dna2) 
    
let _ = Printf.printf "%d\n" (mooshakA dna1 dna2)