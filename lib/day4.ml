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

  let check_xmas_cross_at matrix x y =
    if
      Int.equal x 0 || Int.equal y 0
      || Int.equal y (Array.length matrix - 1)
      || Int.equal x (Array.length (Array.get matrix y) - 1)
      || (not @@ Char.equal 'A' (char_at matrix x y))
    then 0
    else
      (* Check cross figure*)
      let dir1_ok =
        match
          (char_at matrix (x - 1) (y - 1), char_at matrix (x + 1) (y + 1))
        with
        | 'M', 'S' | 'S', 'M' -> true
        | _ -> false
      and dir2_ok =
        match
          (char_at matrix (x - 1) (y + 1), char_at matrix (x + 1) (y - 1))
        with
        | 'M', 'S' | 'S', 'M' -> true
        | _ -> false
      in
      if dir1_ok && dir2_ok then 1 else 0

  let run matrix f =
    Array.foldi matrix ~init:0 ~f:(fun y acc l ->
        acc
        + Array.foldi l ~init:0 ~f:(fun x x_acc _ -> x_acc + f matrix x y) )
    |> print_endline_int

  (* Run part 1 with parsed inputs *)
  let part1 matrix = run matrix count_xmas_from

  (* Run part 2 with parsed inputs *)
  let part2 matrix = run matrix check_xmas_cross_at
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
let%expect_test _ = run example ; [%expect {| 
18 
9
        |}]
