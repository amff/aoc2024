open! Imports
open Base

module M = struct
  (* Type to parse the input into *)
  type t = int * char array

  let index_to_coord (idx : int) (x_len : int) : int * int =
    let y = Int.(idx / x_len) and x = Int.(idx % x_len) in
    (x, y)

  let coord_to_index (x, y) x_len = (y * x_len) + x

  let sum_coord (x, y) (x2, y2) = (x + x2, y + y2)

  let next_dir = function
    | 0, -1 -> (1, 0)
    | 1, 0 -> (0, 1)
    | 0, 1 -> (-1, 0)
    | -1, 0 -> (0, -1)
    | _ -> failwith "invalid"

  (* Parse the input to type t, invoked for both parts *)
  let parse _inputs =
    let lines = String.split_lines _inputs in
    let matrix = List.map lines ~f:String.to_array |> Array.concat in
    let x_len = List.length lines in
    (x_len, matrix)

  let guard_position (x_len, matrix) =
    match Array.findi matrix ~f:(fun _ el -> Char.equal el '^') with
    | None -> failwith "guard"
    | Some (i, _) -> index_to_coord i x_len

  (* Run part 1 with parsed inputs *)
  let part1 (x_len, matrix) =
    let res = Array.create ~len:(Array.length matrix) false in
    let step (x, y) direction =
      let n_x, n_y = sum_coord (x, y) direction in
      if n_x < 0 || n_y < 0 || Int.equal n_x x_len || Int.equal n_y x_len
      then (
        Array.set res (coord_to_index (x, y) x_len) true ;
        None )
      else if
        Char.equal '#' (Array.get matrix (coord_to_index (n_x, n_y) x_len))
      then Some ((x, y), next_dir direction)
      else (
        Array.set res (coord_to_index (x, y) x_len) true ;
        Some ((n_x, n_y), direction) )
    in
    let rec run (x, y) direction =
      match step (x, y) direction with
      | None -> ()
      | Some ((x, y), direction) -> run (x, y) direction
    in
    run (guard_position (x_len, matrix)) (0, -1) ;
    Array.fold res ~init:0 ~f:(fun acc el -> if el then acc + 1 else acc)
    |> Stdlib.print_int

  (* Run part 2 with parsed inputs *)
  let part2 _ = ()
end

include M
include Day.Make (M)

(* Example input *)
let example = ""

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {| |}]
