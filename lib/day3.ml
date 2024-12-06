open! Imports
open Base

type parse_state =
  | Init
  | Mul of int
  | Int1 of int
  | Int2 of int * int
  | Done of int * int
  | Dont of int
  | Do of int
  | Disable
  | Enable

module M = struct
  (* Type to parse the input into *)
  type t = string

  (* Parse the input to type t, invoked for both parts *)
  let parse = Fn.id

  let state_runner allow_dont p_state c enabled =
    let new_state =
      match (p_state, c) with
      | Done (_, _), 'm' | Init, 'm' -> if enabled then Mul 1 else Init
      | Done (_, _), 'd' | Init, 'd' -> if allow_dont then Do 1 else Init
      | Do 1, 'o' -> Do 2
      | Do 2, '(' -> Do 3
      | Do 2, 'n' -> Dont 1
      | Do 3, ')' -> Enable
      | Dont 1, '\'' -> Dont 2
      | Dont 2, 't' -> Dont 3
      | Dont 3, '(' -> Dont 4
      | Dont 4, ')' -> Disable
      | Mul 1, 'u' -> Mul 2
      | Mul 1, 'm' -> Mul 1
      | Mul 2, 'l' -> Mul 3
      | Mul 2, 'm' -> Mul 1
      | Mul 3, '(' -> Int1 0
      | Mul 3, 'm' -> Mul 1
      | Int1 i, c ->
          if Char.is_digit c && i < 100 then
            Int1 ((i * 10) + Char.get_digit_exn c)
          else if Char.equal c ',' && (not @@ Int.equal 0 i) then Int2 (i, 0)
          else if Char.equal c 'm' then Mul 1
          else if Char.equal c 'd' && allow_dont then Do 1
          else Init
      | Int2 (i1, i), c ->
          if Char.is_digit c && i < 100 then
            Int2 (i1, (i * 10) + Char.get_digit_exn c)
          else if Char.equal c ')' && (not @@ Int.equal 0 i) then Done (i1, i)
          else if Char.equal c 'm' then Mul 1
          else if Char.equal c 'd' && allow_dont then Do 1
          else Init
      | _, _ -> Init
    in
    match new_state with
    | Enable -> (Init, true)
    | Disable -> (Init, false)
    | _ -> (new_state, enabled)

  let run allow_dont text =
    let _, _, total =
      String.fold text ~init:(true, Init, 0)
        ~f:(fun (enabled, p_state, total) c ->
          let new_state, enabled =
            state_runner allow_dont p_state c enabled
          in
          match new_state with
          | Done (i1, i2) -> (enabled, new_state, total + (i1 * i2))
          | _ -> (enabled, new_state, total) )
    in
    print_endline_int total

  (* Run part 1 with parsed inputs *)
  let part1 text = run false text

  (* Run part 2 with parsed inputs *)
  let part2 text = run true text
end

include M
include Day.Make (M)

(* Example input *)
let example =
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {| 
161
161
          |}]
