let input_folder = "input"

let parse_file' file_name =
  let rec lists l ic =
    try
      let line = input_line ic in
      let parts = Str.split (Str.regexp " ") line in
      lists (List.map int_of_string parts :: l) ic
    with End_of_file ->
      close_in ic;
      l
  in
  let ic = open_in (input_folder ^ "/" ^ file_name) in
  lists [] ic


let parse_file file_name =
  let rec lists l1 l2 ic = 
    try
      let line = input_line ic in
      let parts = Str.split (Str.regexp "   ") line in
      let n1, n2 = List.nth parts 0, List.nth parts 1 in
      lists ((int_of_string n1) :: l1) ((int_of_string n2) :: l2) ic
    with End_of_file ->
      close_in ic;
      l1, l2
  in
  let ic = open_in (input_folder ^ "/" ^ file_name) in
  lists [] [] ic

let parse_file'' file_name =
  let rec lists l ic =
    try
      let line = input_line ic in
      lists (line :: l) ic
    with End_of_file ->
      close_in ic;
      l
  in
  let ic = open_in (input_folder ^ "/" ^ file_name) in
  lists [] ic
