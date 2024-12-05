open! Imports
open Base

module M = struct
  (* Type to parse the input into *)
  type t = int list * int list

  (* Parse the input to type t, invoked for both parts *)
  let parse _inputs =
    String.split_lines _inputs
    |> List.map ~f:(fun line ->
           Stdlib.Scanf.sscanf line "%d %d" (fun i1 i2 -> (i1, i2)) )
    |> Stdlib.List.split

  (* Run part 1 with parsed inputs *)
  let part1 (first, second) =
    let sorted_first = List.sort first ~compare:Int.compare
    and sorted_second = List.sort second ~compare:Int.compare in
    let rec s_aux l1 l2 acc =
      match (l1, l2) with
      | [], [] -> acc
      | [], _ | _, [] -> failwith "invalid input"
      | i1 :: t1, i2 :: t2 -> s_aux t1 t2 (acc + abs (i1 - i2))
    in
    Stdio.print_endline
    @@ (s_aux sorted_first sorted_second 0 |> Int.to_string)

  (* Run part 2 with parsed inputs *)
  let part2 (first, second) =
    List.map first ~f:(fun x -> x * List.count second ~f:(fun i -> i = x))
    |> List.fold ~init:0 ~f:Int.( + )
    |> Int.to_string |> Stdio.print_endline
end

include M
include Day.Make (M)

(* Example input *)
let example = "3 5\n5 1"

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {| 
    2 
    5 
      |}]
