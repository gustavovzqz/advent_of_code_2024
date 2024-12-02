let init_by_file path =
  let file = open_in path in
  let rec read_file l =
    try
      let line = input_line file in
      line :: read_file l
    with End_of_file -> l
  in
  read_file []

let () =
  let l1 = init_by_file "input.txt" in
  List.iter print_endline l1
