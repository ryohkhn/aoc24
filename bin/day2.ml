let rec is_sorted cmp = function
  | fst :: snd :: l when cmp fst snd -> is_sorted cmp (snd :: l)
  | [] | _ :: [] -> true
  | _ -> false

let nb_safe a b =
  let diff = abs(a - b) in
  diff > 0 && diff < 4

let rec is_safe = function
  | fst :: snd :: l  -> nb_safe fst snd && is_safe (snd :: l)
  | _ -> true

let is_safish l =
  let rec is_safish' acc l' = function
    | _ when acc > 1 -> [], false
    | fst :: snd :: l -> (
        if not (nb_safe fst snd) then
          is_safish' (acc + 1) (fst :: l') (fst :: l)
        else
          is_safish' acc (snd :: l') (snd :: l)
      )
    | _ -> List.rev l', true
  in
  is_safish' 0 [] l

let part1 l =
  List.fold_left (fun acc l ->
      acc + (((is_sorted (<) l || is_sorted (>) l) && is_safe l) |> Bool.to_int)
  ) 0 l

let part2 l =
  List.fold_left (fun acc l ->
      let (l', b) = is_safish l in
      List.iter (fun a -> Printf.printf "%d " a) l';
      print_endline " safe";
      acc + ((b && (is_sorted (<) l' || is_sorted (>) l')) |> Bool.to_int)
  ) 0 l

let run =
  let l = Utils.parse_file' "day2.input" in
  part1 l |> Printf.printf "part1: %d\n";
  part2 l |> Printf.printf "part2: %d\n"
