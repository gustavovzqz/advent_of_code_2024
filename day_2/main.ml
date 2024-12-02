let parse_line line = List.map int_of_string (String.split_on_char ' ' line)

let init_by_file path =
  let file = open_in path in
  let rec read_file l =
    try
      let line = input_line file in
      let numbers = parse_line line in
      read_file (numbers :: l)
    with End_of_file -> l
  in
  read_file []

let rec list_to_string list =
  match list with
  | [] -> ""
  | h :: t -> string_of_int h ^ " " ^ list_to_string t

(*
  Tem uma forma bem mais simples de fazer,
  zipando a lista em pares (a, b) e vendo se todos os pares são decrescentes / crescentes.
  depois basta verificar se a remoção de algum torna a lista da forma acima (caso não seja)
*)

let rec increasing l =
  match l with
  | h1 :: h2 :: t -> h2 - h1 <= 3 && h2 - h1 > 0 && increasing (h2 :: t)
  | _ -> true

let rec decreasing l =
  match l with
  | h1 :: h2 :: t -> h1 - h2 <= 3 && h1 - h2 > 0 && decreasing (h2 :: t)
  | _ -> true

let rec increasing_foda l prev =
  match l with
  | h1 :: h2 :: t when h2 - h1 <= 3 && h2 - h1 > 0 ->
      increasing_foda (h2 :: t) (prev @ [ h1 ])
  | h1 :: h2 :: t ->
      increasing (prev @ (h1 :: t)) || increasing (prev @ (h2 :: t))
  | _ -> true

let rec decreasing_foda l prev =
  match l with
  | h1 :: h2 :: t when h1 - h2 <= 3 && h1 - h2 > 0 ->
      decreasing_foda (h2 :: t) (prev @ [ h1 ])
  | h1 :: h2 :: t ->
      decreasing (prev @ (h1 :: t)) || decreasing (prev @ (h2 :: t))
  | _ -> true

let rec first_part l1 count =
  match l1 with
  | [] -> count
  | l1 :: t when increasing l1 || decreasing l1 -> first_part t (count + 1)
  | _ :: t -> first_part t count

let rec second_part l1 count =
  match l1 with
  | [] -> count
  | l1 :: t when increasing_foda l1 [] || decreasing_foda l1 [] ->
      Printf.printf "%s I:%b D:%b\n" (list_to_string l1) (increasing_foda l1 [])
        (decreasing_foda l1 []);
      second_part t (count + 1)
  | _ :: t -> second_part t count

let () =
  let list = init_by_file "input.txt" in
  Printf.printf "First part solution: %d\n" (first_part list 0);
  Printf.printf "Second part solution: %d\n" (second_part list 0)
