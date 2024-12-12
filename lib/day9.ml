open! Imports
open Base

module M = struct
  (* Type to parse the input into *)
  type e = File of int * int * int | Gap | Free

  type t = e Array.t

  let is_file e = match e with File (_, _, _) -> true | _ -> false

  (* Parse the input to type t, invoked for both parts *)
  let parse _inputs =
    let rec go lst is_free_space cur_id cur_index =
      match lst with
      | [] -> []
      | elem :: rst ->
          let repeat_n = Char.get_digit_exn elem in
          if not is_free_space then
            List.init repeat_n ~f:(fun _ ->
                File (cur_id, repeat_n, cur_index) )
            :: go rst (not is_free_space) cur_id (cur_index + repeat_n)
          else
            List.init repeat_n ~f:(fun _ -> Gap)
            :: go rst (not is_free_space) (cur_id + 1) (cur_index + repeat_n)
    in
    go (String.to_list _inputs) false 0 0 |> List.concat |> Array.of_list

  let check_sum =
    Array.foldi ~init:0 ~f:(fun idx acc c ->
        match c with File (c, _, _) -> acc + (c * idx) | _ -> acc )

  let compact fsa =
    let front_idx = ref 0 and back_idx = ref (Array.length fsa - 1) in
    while !front_idx < !back_idx do
      let front_elem = Array.get fsa !front_idx
      and back_elem = Array.get fsa !back_idx in
      match (front_elem, back_elem) with
      | File _, _ ->
          front_idx := !front_idx + 1 (* front index forward until gap *)
      | Gap, Gap -> back_idx := !back_idx - 1
      | Gap, File (id, len, start_idx) ->
          Array.set fsa !front_idx (File (id, len, start_idx)) ;
          Array.set fsa !back_idx Free ;
          front_idx := !front_idx + 1 ;
          back_idx := !back_idx - 1
      | _ -> failwith "unreachable"
    done

  let gap_from arr n =
    let cur_idx = ref n and end_gap = ref false in
    let arr_len = Array.length arr in
    let () =
      while (not !end_gap) && !cur_idx < arr_len do
        match Array.get arr !cur_idx with
        | Free | Gap -> cur_idx := !cur_idx + 1
        | _ -> end_gap := true
      done
    in
    if !cur_idx >= arr_len then 0 else !cur_idx - n

  let file_equal a b =
    match (a, b) with
    | File (id1, _, _), File (id2, _, _) -> Int.equal id1 id2
    | _ -> false

  let file_compare a b =
    match (a, b) with
    | File (id1, _, _), File (id2, _, _) -> Int.compare id1 id2
    | File (_, _, _), Gap -> 1
    | File (_, _, _), Free -> 1
    | _, _ -> 0

  let find_file_to_move file_index unmoved_files gap_size =
    let file_id = ref (Hashtbl.length file_index - 1) in
    let found = ref None in
    while !file_id > 0 && Option.is_none !found do
      let len, start_idx = Hashtbl.find_exn file_index !file_id in
      if
        Hash_set.exists unmoved_files ~f:(Int.equal !file_id)
        && len <= gap_size
      then found := Some (!file_id, (len, start_idx))
      else file_id := !file_id - 1
    done ;
    !found

  let defrag fsa =
    let rec move_file front_idx file_idx left =
      if Int.equal left 0 then ()
      else (
        Array.set fsa front_idx (Array.get fsa file_idx) ;
        Array.set fsa file_idx Free ;
        move_file (front_idx + 1) (file_idx + 1) (left - 1) )
    and front_idx = ref 0
    and disk_length = Array.length fsa
    and files =
      Array.filter fsa ~f:is_file
      |> Array.fold ~init:[] ~f:(fun acc x ->
             if List.mem acc x ~equal:file_equal then acc else x :: acc )
      |> List.sort ~compare:file_compare
      |> List.map ~f:(fun elem ->
             match elem with
             | File (id, len, start_index) -> (id, (len, start_index))
             | _ -> failwith "unreachable" )
    in
    let file_index = Hashtbl.of_alist_exn (module Int) files
    and unmoved_files =
      Hash_set.of_list (module Int) (List.map files ~f:fst)
    in
    let () =
      while !front_idx < disk_length do
        let f_elem = Array.get fsa !front_idx in
        match f_elem with
        | File (cur_id, len, _) ->
            front_idx := !front_idx + len ;
            (* Free won't happen *)
            Hash_set.remove unmoved_files cur_id
        | Free | Gap -> (
            let gap_size = gap_from fsa !front_idx in
            match find_file_to_move file_index unmoved_files gap_size with
            | Some (id, (len, start_idx)) ->
                move_file !front_idx start_idx len ;
                front_idx := !front_idx + len ;
                Hash_set.remove unmoved_files id
            | None -> front_idx := !front_idx + gap_size )
      done
    in
    ()

  (* Run part 1 with parsed inputs *)
  let part1 input =
    compact input ;
    check_sum input |> Stdlib.print_int |> Stdlib.print_newline

  (* Run part 2 with parsed inputs *)
  let part2 input =
    defrag input ;
    check_sum input |> Stdlib.print_int
end

include M
include Day.Make (M)

(* Example input *)
let example = ""

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {| |}]
