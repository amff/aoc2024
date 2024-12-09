open! Imports
open Base

module M = struct
  (* Type to parse the input into *)
  type t = int option list

  type n = Gap of int | File of int * int

  (* Parse the input to type t, invoked for both parts *)
  let parse _inputs =
    let rec go lst is_free_space cur_id =
      match lst with
      | [] -> []
      | elem :: rst ->
          let repeat_n = Char.get_digit_exn elem in
          if not is_free_space then
            List.init repeat_n ~f:(fun _ -> Some cur_id)
            :: go rst (not is_free_space) cur_id
          else
            List.init repeat_n ~f:(fun _ -> None)
            :: go rst (not is_free_space) (cur_id + 1)
    in
    go (String.to_list _inputs) false 0 |> List.concat

  let parse2 inputs =
    let rec go lst is_free_space cur_id =
      match lst with
      | [] -> []
      | elem :: rst ->
          if not is_free_space then
            File (cur_id, elem) :: go rst (not is_free_space) cur_id
          else Gap elem :: go rst (not is_free_space) (cur_id + 1)
    in
    go inputs false 0

  let check_sum fs_list =
    List.foldi fs_list ~init:0 ~f:(fun idx acc c_opt ->
        match c_opt with None -> acc | Some c -> acc + (c * idx) )

  let defrag fs_list =
    let () = Stdlib.print_endline "Start defrag" in
    let a = Array.of_list fs_list
    and front_idx = ref 0
    and back_idx = ref (List.length fs_list - 1) in
    let () =
      while !front_idx < !back_idx do
        let front_elem = Array.get a !front_idx
        and back_elem = Array.get a !back_idx in
        match (front_elem, back_elem) with
        | Some _, _ ->
            front_idx := !front_idx + 1 (* front index forward until gap *)
        | None, None ->
            back_idx :=
              !back_idx - 1 (* back index back until letter to defrag *)
        | None, Some n ->
            Array.set a !front_idx (Some n) ;
            Array.set a !back_idx None (* swap *)
      done
    in
    Array.to_list a

  (* Run part 1 with parsed inputs *)
  let part1 input =
    defrag input |> check_sum |> Stdlib.print_int |> Stdlib.print_newline

  (* Run part 2 with parsed inputs *)
  let part2 _ = ()
end

include M
include Day.Make (M)

(* Example input *)
let example = ""

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {| |}]
