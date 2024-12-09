open! Imports
open Base

module M = struct
  (* Type to parse the input into *)
  type t = int * (char, int list, Char.comparator_witness) Map.t

  let is_coord_in_grid grid_size (x, y) =
    if x < 0 || y < 0 || x >= grid_size || y >= grid_size then false
    else true

  let antinodes_for with_ressonant_harmonics grid_size (a, b) =
    let s = Sequence.unfold ~init:0 ~f:(fun n -> Some (n, n)) in
    let x1, y1 = Day6.index_to_coord a grid_size
    and x2, y2 = Day6.index_to_coord b grid_size in
    let dx = x2 - x1 and dy = y2 - y1 in
    let anti_nodes =
      if not with_ressonant_harmonics then
        [(x1 - dx, y1 - dy); (x2 + dx, y2 + dy)]
      else
        Sequence.fold_until s ~init:[]
          ~f:(fun acc r ->
            let c1 = (x1 - (r * dx), y1 - (r * dx))
            and c2 = (x2 + (r * dx), y2 + (r * dy)) in
            let c1_ok = is_coord_in_grid grid_size c1
            and c2_ok = is_coord_in_grid grid_size c2 in
            if not (c1_ok && c2_ok) then Stop acc
            else
              Continue
                ( [c1; c2]
                |> List.filter ~f:(is_coord_in_grid grid_size)
                |> List.append acc ) )
          ~finish:Fn.id
    in
    List.filter anti_nodes ~f:(is_coord_in_grid grid_size)
    |> List.map ~f:(Day6.coord_to_index grid_size)

  let make_pairs lst =
    let rec go lst =
      match lst with
      | _ :: [] | [] -> []
      | cur :: rst -> List.map rst ~f:(fun x -> (cur, x)) :: go rst
    in
    List.concat (go lst)

  (* Parse the input to type t, invoked for both parts *)
  let parse _inputs =
    let lines = String.split_lines _inputs in
    let grid_dimension = List.length lines in
    let rec go acc idx remaining =
      match remaining with
      | [] -> acc
      | elem :: rst ->
          if Char.is_alphanum elem then go ((elem, idx) :: acc) (idx + 1) rst
          else if not (Char.equal elem '\n') then go acc (idx + 1) rst
          else go acc idx rst
    in
    let antennas = go [] 0 (String.to_list _inputs) in
    (grid_dimension, Map.of_alist_multi (module Char) antennas)

  (* Run part 1 with parsed inputs *)
  let part1 (grid_size, antennas) =
    Map.map antennas ~f:(fun v ->
        make_pairs v
        |> List.concat_map ~f:(fun pair ->
               antinodes_for false grid_size pair ) )
    |> Map.to_alist |> List.concat_map ~f:snd
    |> Set.of_list (module Int)
    |> Set.length |> Stdlib.print_int |> Stdlib.print_newline

  (* Run part 2 with parsed inputs *)
  let part2 (grid_size, antennas) =
    Map.map antennas ~f:(fun v ->
        make_pairs v
        |> List.concat_map ~f:(fun pair -> antinodes_for true grid_size pair) )
    |> Map.to_alist |> List.concat_map ~f:snd
    |> Set.of_list (module Int)
    |> Set.length |> Stdlib.print_int
end

include M
include Day.Make (M)

(* Example input *)
let example = ""

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {| |}]
