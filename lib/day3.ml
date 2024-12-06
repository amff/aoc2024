open! Imports
open Base

type parse_state =
  | Init
  | Mul of int
  | Int1 of int
  | Int2 of int * int
  | Done of int * int

module M = struct
  (* Type to parse the input into *)
  type t = string

  (* Parse the input to type t, invoked for both parts *)
  let parse = Fn.id

  let state_runner = function
    | Done (_, _), 'm' -> Mul 1
    | Init, 'm' -> Mul 1
    | Mul 1, 'u' -> Mul 2
    | Mul 1, 'm' -> Mul 1
    | Mul 2, 'l' -> Mul 3
    | Mul 2, 'm' -> Mul 1
    | Mul 3, '(' -> Int1 0
    | Mul 3, 'm' -> Mul 1
    | Int1 i, c ->
        if Char.is_digit c && i < 100 then Int1 ((i * 10) + Char.to_int c)
        else if Char.equal c ',' && (not @@ Int.equal 0 i) then Int2 (i, 0)
        else if Char.equal c 'm' then Mul 1
        else Init
    | Int2 (i1, i), c ->
        if Char.is_digit c && i < 100 then Int2 (i1, (i * 10) + Char.to_int c)
        else if Char.equal c ')' && (not @@ Int.equal 0 i) then Done (i1, i)
        else if Char.equal c 'm' then Mul 1
        else Init
    | _, _ -> Init

  (* Run part 1 with parsed inputs *)
  let part1 text =
    let _, total =
      String.fold text ~init:(Init, 0) ~f:(fun (p_state, total) c ->
          let new_state = state_runner (p_state, c) in
          match new_state with
          | Done (i1, i2) -> (new_state, total + (i1 * i2))
          | _ -> (new_state, total) )
    in
    print_endline_int total

  (* Run part 2 with parsed inputs *)
  let part2 _ = ()
end

include M
include Day.Make (M)

(* Example input *)
let example =
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {| 161 |}]
