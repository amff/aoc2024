open! Imports
open Base

module M = struct
  (* Type to parse the input into *)
  type t = int * char array

  type step_result = Out | Loop | Step of (int * int) * (int * int)

  let index_to_coord (idx : int) (x_len : int) : int * int =
    let y = Int.(idx / x_len) and x = Int.(idx % x_len) in
    (x, y)

  let coord_to_index (x, y) x_len = (y * x_len) + x

  let sum_coord (x, y) (x2, y2) = (x + x2, y + y2)

  let next_dir = function
    | 0, -1 -> (1, 0)
    | 1, 0 -> (0, 1)
    | 0, 1 -> (-1, 0)
    | -1, 0 -> (0, -1)
    | _ -> failwith "invalid"

  (* Parse the input to type t, invoked for both parts *)
  let parse _inputs =
    let lines = String.split_lines _inputs in
    let matrix = List.map lines ~f:String.to_array |> Array.concat in
    let x_len = List.length lines in
    (x_len, matrix)

  let guard_position (x_len, matrix) =
    match Array.findi matrix ~f:(fun _ el -> Char.equal el '^') with
    | None -> failwith "guard"
    | Some (i, _) -> index_to_coord i x_len

  let check_guard_path (x_len, matrix) =
    let res = Array.create ~len:(Array.length matrix) None in
    let step (x, y) direction =
      let dx, dy = direction in
      let n_x, n_y = sum_coord (x, y) direction in
      let new_idx = coord_to_index (n_x, n_y) x_len in
      if n_x < 0 || n_y < 0 || Int.equal n_x x_len || Int.equal n_y x_len
      then
        (* Array.set res (coord_to_index (x, y) x_len) (Some direction) ; *)
        Out
      else if Char.equal '#' (Array.get matrix new_idx) then
        Step ((x, y), next_dir direction)
      else
        (* Array.set res (coord_to_index (x, y) x_len) (Some direction) ; *)
        match Array.get res new_idx with
        | Some (vdx, vdy) ->
            if Int.equal dx vdx && Int.equal dy vdy then Loop
            else (
              Array.set res new_idx (Some direction) ;
              Step ((n_x, n_y), direction) )
        | _ ->
            Array.set res new_idx (Some direction) ;
            Step ((n_x, n_y), direction)
    in
    let rec run (x, y) direction =
      match step (x, y) direction with
      | Out -> Out
      | Loop -> Loop
      | Step ((x, y), direction) -> run (x, y) direction
    in
    match run (guard_position (x_len, matrix)) (0, -1) with
    | Loop -> (Loop, res)
    | Out -> (Out, res)
    | _ -> failwith "should not occur"

  (* Run part 1 with parsed inputs *)
  let part1 input =
    check_guard_path input |> snd
    |> Array.fold ~init:0 ~f:(fun acc el ->
           if Option.is_some el then acc + 1 else acc )
    |> Stdlib.print_int

  (* Run part 2 with parsed inputs *)
  let part2 input =
    let _, matrix = input in
    let gx, gy = guard_position input in
    let path = check_guard_path input in
    let visited_idxs =
      snd path
      |> Array.filter_mapi ~f:(fun idx elem ->
             match elem with
             | Some (x, y) ->
                 if not (Int.equal x gx && Int.equal y gy) then Some idx
                 else None
             | None -> None )
    in
    Array.fold visited_idxs ~init:0 ~f:(fun acc elem ->
        (* ugly ass mutation *)
        Array.set matrix elem '#' ;
        let res =
          match check_guard_path input |> fst with
          | Loop -> acc + 1
          | _ -> acc
        in
        Array.set matrix elem '.' ; res )
    |> Stdlib.print_int
end

include M
include Day.Make (M)

(* Example input *)
(*
let example = ""

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {| |}]
*)
