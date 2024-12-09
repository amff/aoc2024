open! Imports
open Base

module M = struct
  (* Type to parse the input into *)
  type t = int option list

  (* Parse the input to type t, invoked for both parts *)
  let parse _inputs =
    let rec go (acc : int option list) lst is_free_space cur_id =
      match lst with
      | [] -> acc
      | elem :: rst ->
          let repeat_n = Char.get_digit_exn elem in
          if not is_free_space then
            go
              (List.append acc
                 (List.init repeat_n ~f:(fun _ -> Some cur_id)) )
              rst (not is_free_space) cur_id
          else
            go
              (List.append acc (List.init repeat_n ~f:(fun _ -> None)))
              rst (not is_free_space) (cur_id + 1)
    in
    go [] (String.to_list _inputs) false 0

  let check_sum fs_list =
    List.foldi fs_list ~init:0 ~f:(fun idx acc c_opt ->
        match c_opt with
        | None -> acc
        | Some c -> acc + (Char.get_digit_exn c * idx) )

  (* Run part 1 with parsed inputs *)
  let part1 _ = ()

  (* Run part 2 with parsed inputs *)
  let part2 _ = ()
end

include M
include Day.Make (M)

(* Example input *)
let example = ""

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {| |}]
