open! Imports
open Base

module M = struct
  (* Type to parse the input into *)
  type t = (int * int list) list

  (* Parse the input to type t, invoked for both parts *)
  let parse _inputs =
    String.split_lines _inputs
    |> List.map ~f:(fun line ->
           String.split_on_chars line ~on:[':'; ' ']
           |> List.filter ~f:(fun e -> not @@ String.is_empty e)
           |> List.map ~f:Int.of_string
           |> fun line -> (List.hd_exn line, List.tl_exn line) )

  let check_equation test_value numbers with_concat =
    let rec go acc remaining =
      match remaining with
      | [] -> Int.equal acc test_value
      | elem :: rst ->
          go (elem + acc) rst
          || go (elem * acc) rst
          || with_concat
             && go (Int.of_string (Printf.sprintf "%d%d" acc elem)) rst
    in
    go (List.hd_exn numbers) (List.tl_exn numbers)

  (* Run part 1 with parsed inputs *)
  let part1 equations =
    List.filter equations ~f:(fun equation ->
        check_equation (fst equation) (snd equation) false)
    |> List.fold ~init:0 ~f:(fun acc equation -> acc + fst equation)
    |> Stdlib.print_int |> Stdlib.print_newline

  (* Run part 2 with parsed inputs *)
  let part2 equations = 
    List.filter equations ~f:(fun equation ->
        check_equation (fst equation) (snd equation) true)
    |> List.fold ~init:0 ~f:(fun acc equation -> acc + fst equation)
    |> Stdlib.print_int |> Stdlib.print_newline
end

include M
include Day.Make (M)

(* Example input *)
let example =
  "190: 10 19\n\
   3267: 81 40 27\n\
   83: 17 5\n\
   156: 15 6\n\
   7290: 6 8 6 15\n\
   161011: 16 10 13\n\
   192: 17 8 14\n\
   21037: 9 7 18 13\n\
   292: 11 6 16 20"

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {| 
    3749
    11387 |}]
