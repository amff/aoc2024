open! Imports
open Base

module M = struct
  (* Type to parse the input into *)
  type t = int * (char, int list, Char.comparator_witness) Map.t

  let is_coord_in_grid grid_size (x, y) =
    if x < 0 || y < 0 || x >= grid_size || y >= grid_size then false
    else true

  let antinodes_for grid_size (a, b) =
    let x1, y1 = Day6.index_to_coord a grid_size
    and x2, y2 = Day6.index_to_coord b grid_size in
    let dx = x2 - x1 and dy = y2 - y1 in
    [(x1 - dx, y1 - dy); (x2 + dx, y2 + dy)]
    |> List.filter ~f:(is_coord_in_grid grid_size)
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
    Stdio.printf "%d" grid_size ;
    Map.map antennas ~f:(fun v ->
        make_pairs v
        |> List.concat_map ~f:(fun pair -> antinodes_for grid_size pair) )
    |> Map.to_alist |> List.concat_map ~f:snd
    |> Set.of_list (module Int)
    |> Set.length |> Stdlib.print_int

  (* Run part 2 with parsed inputs *)
  let part2 _ = ()
end

include M
include Day.Make (M)

(* Example input *)
let example = ""

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {| |}]
