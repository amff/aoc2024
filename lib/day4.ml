open! Imports
open Base

module M = struct
  (* Type to parse the input into *)
  type t = char array array

  (* Parse the input to type t, invoked for both parts *)
  let parse _inputs =
    String.split_lines _inputs
    |> List.map ~f:String.to_array
    |> Array.of_list

  let char_at matrix x y =
    let line = Array.get matrix y in
    Array.get line x

  let count_xmas_from matrix x y =
    let rec check_aux (x_dir, y_dir) depth x y =
      if
        x < 0 || y < 0
        || y >= Array.length matrix
        || x >= Array.length (Array.get matrix y)
      then false
      else
        match (depth, char_at matrix x y) with
        | 3, 'S' -> true
        | 0, 'X' | 1, 'M' | 2, 'A' ->
            check_aux (x_dir, y_dir) (depth + 1) (x + x_dir) (y + y_dir)
        | _ -> false
    and directions =
      [(0, -1); (0, 1); (-1, 0); (1, 0); (-1, -1); (1, -1); (-1, 1); (1, 1)]
    in
    List.fold directions ~init:0 ~f:(fun acc dir ->
        if check_aux dir 0 x y then acc + 1 else acc )

  (* Run part 1 with parsed inputs *)
  let part1 matrix =
    Array.foldi matrix ~init:0 ~f:(fun y acc l ->
        acc
        + Array.foldi l ~init:0 ~f:(fun x x_acc _ ->
              x_acc + count_xmas_from matrix x y ) )
    |> print_endline_int

  (* Run part 2 with parsed inputs *)
  let part2 _ = ()
end

include M
include Day.Make (M)

(* Example input *)
let example =
  "MMMSXXMASM\n\
   MSAMXMSMSA\n\
   AMXSXMAAMM\n\
   MSAMASMSMX\n\
   XMASAMXAMM\n\
   XXAMMXXAMA\n\
   SMSMSASXSS\n\
   SAXAMASAAA\n\
   MAMMMXMMMM\n\
   MXMXAXMASX"

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {| 18 |}]
