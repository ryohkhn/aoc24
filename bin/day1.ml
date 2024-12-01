let part1 l1 l2 =
  List.fold_left2 (fun acc a b -> acc + abs(a - b)) 0 l1 l2

let part2 l1 l2 =
  List.map (fun a -> a * List.length (List.filter (fun n -> a = n) l2)) l1
  |> List.fold_left (+) 0

let run =
  let l1, l2 = Utils.parse_file "day1.data" in
  let l1, l2 = (List.sort compare l1, List.sort compare l2) in
  part1 l1 l2 |> Printf.printf "part1: %d\n";
  part2 l1 l2 |> Printf.printf "part2: %d\n";
