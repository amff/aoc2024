open! Imports
open Base

module M = struct
  (* Type to parse the input into *)
  type t = int list list

  type direction = Ascending | Descending

  (* Parse the input to type t, invoked for both parts *)
  let parse _inputs =
    String.split_lines _inputs
    |> List.map ~f:(fun line ->
           String.split line ~on:' ' |> List.map ~f:Int.of_string )

  let rec s_aux (last_n, dir) rst =
    match rst with
    | [] -> true
    | fst :: tl ->
        let diff = fst - last_n in
        if diff > 0 && diff < 4 && phys_equal dir Ascending then
          s_aux (fst, dir) tl
        else if diff < 0 && diff > -4 && phys_equal dir Descending then
          s_aux (fst, dir) tl
        else false

  let is_report_safe = function
    | fst :: snd :: tl ->
        let diff = snd - fst in
        if diff > 0 && diff < 4 then s_aux (snd, Ascending) tl
        else if diff < 0 && diff > -4 then s_aux (snd, Descending) tl
        else false
    | _ -> failwith "invalid report"

  (* Run part 1 with parsed inputs *)
  let part1 input =
    let report_safety_list = List.map input ~f:is_report_safe in
    Stdio.print_endline
    @@ (List.count report_safety_list ~f:Fn.id |> Int.to_string)

  (* Run part 2 with parsed inputs *)
  let part2 _ = ()
end

include M
include Day.Make (M)

(* Example input *)
let example =
  "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9"

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {| 2 
        4 |}]
