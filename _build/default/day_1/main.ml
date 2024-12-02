let parse_line line =
  let int_string = List.map int_of_string (String.split_on_char ' ' line) in

  match int_string with [ v1; v2 ] -> (v1, v2) | _ -> assert false

let init_by_file path =
  let file = open_in path in
  let rec read_file (l1, l2) =
    try
      let line = input_line file in
      let left_value, right_value = parse_line line in
      read_file (left_value :: l1, right_value :: l2)
    with End_of_file -> (l1, l2)
  in
  read_file ([], [])

let rec sum_differences_lists l1 l2 =
  match (l1, l2) with
  | h1 :: t1, h2 :: t2 -> abs (h1 - h2) + sum_differences_lists t1 t2
  | [], [] -> 0
  | _ -> assert false

let count elem list =
  let rec count_iter list count =
    match list with
    | [] -> count
    | h :: t when h = elem -> count_iter t (count + 1)
    | _ :: t -> count_iter t count
  in
  count_iter list 0

let rec second_part l1 l2 =
  match l1 with [] -> 0 | h :: t -> (h * count h l2) + second_part t l2

let () =
  let list_1, list_2 = init_by_file "input.txt" in

  let list_1_sorted = List.sort compare list_1
  and list_2_sorted = List.sort compare list_2 in

  Printf.printf "Resposta da primeira parte: %d\n"
    (sum_differences_lists list_1_sorted list_2_sorted);

  Printf.printf "Resposta da segunda parte: %d\n" (second_part list_1 list_2)
