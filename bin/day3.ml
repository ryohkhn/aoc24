open Utils

let mul l pattern =
  List.fold_left (fun acc s ->
    let re = Re.compile pattern in
    let mul = Re.all re s |>
    List.map (fun g ->
      let first_value = Re.Group.get g 1 |> int_of_string in
      let second_value = Re.Group.get g 2 |> int_of_string in
      first_value * second_value
    )
    in
    acc + (List.fold_left (+) 0 mul)
  ) 0 l

let part_1 l =
  let pattern = Re.Perl.re {|mul\((\d{1,3}),(\d{1,3})\)|} in
  mul l pattern

let mul' l pattern =
  let count = ref true in
  let re = Re.compile pattern in
  List.fold_right (fun s acc ->
      let mul =
        let l = Re.all re s in
        List.fold_left (fun sub_acc g ->
            match Re.Group.get g 0 with
            | "do()" ->
              count := true;
              sub_acc
            | "don't()" ->
              count := false;
              sub_acc
            | _ when !count ->
              let first_value = Re.Group.get g 1 |> int_of_string in
              let second_value = Re.Group.get g 2 |> int_of_string in
              sub_acc + first_value * second_value
            | _ -> sub_acc
          ) 0 l
      in
      acc + mul
    ) l 0

let part_2 l =
  let pattern = Re.Perl.re {|mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)|} in
  mul' l pattern

let run =
  let l = parse_file'' "day3.input" in
  part_1 l |> Printf.printf "part1: %d\n";
  part_2 l |> Printf.printf "part2: %d\n"
