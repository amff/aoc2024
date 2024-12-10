open! Imports
open Base

module M = struct
  (* Type to parse the input into *)
  type e = File of int * int | Gap | Free

  type t = e Array.t

  let is_file e = match e with File (_, _) -> true | _ -> false

  (* Parse the input to type t, invoked for both parts *)
  let parse _inputs =
    let rec go lst is_free_space cur_id =
      match lst with
      | [] -> []
      | elem :: rst ->
          let repeat_n = Char.get_digit_exn elem in
          if not is_free_space then
            List.init repeat_n ~f:(fun _ -> File (cur_id, repeat_n))
            :: go rst (not is_free_space) cur_id
          else
            List.init repeat_n ~f:(fun _ -> Gap)
            :: go rst (not is_free_space) (cur_id + 1)
    in
    go (String.to_list _inputs) false 0 |> List.concat |> Array.of_list

  let check_sum =
    Array.foldi ~init:0 ~f:(fun idx acc c ->
        match c with File (c, _) -> acc + (c * idx) | _ -> acc )

  let compact fsa =
    let front_idx = ref 0 and back_idx = ref (Array.length fsa - 1) in
    while !front_idx < !back_idx do
      let front_elem = Array.get fsa !front_idx
      and back_elem = Array.get fsa !back_idx in
      match (front_elem, back_elem) with
      | File _, _ ->
          front_idx := !front_idx + 1 (* front index forward until gap *)
      | Gap, Gap ->
          back_idx :=
            !back_idx - 1 (* back index back until letter to defrag *)
      | Gap, File (id, len) ->
          Array.set fsa !front_idx (File (id, len)) ;
          Array.set fsa !back_idx Free ;
          front_idx := !front_idx + 1 ;
          back_idx := !back_idx - 1
      | _ -> failwith "unreachable"
    done

  let rec gap_from arr n =
    match Array.get arr n with
    | Free | Gap -> 1 + (gap_from arr n + 1)
    | _ -> 0

  let defrag fsa =
    let files = Array.filter fsa ~f:is_file in
    _

  (* Run part 1 with parsed inputs *)
  let part1 input =
    compact input ;
    check_sum input |> Stdlib.print_int |> Stdlib.print_newline

  (* Run part 2 with parsed inputs *)
  let part2 _ = ()
end

include M
include Day.Make (M)

(* Example input *)
let example = ""

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {| |}]
