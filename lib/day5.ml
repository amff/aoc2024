open! Imports
open Base

module M = struct
  type rule = int * int

  (* Type to parse the input into *)
  type t = rule list * int list list

  let parse_rules r_lines =
    List.map r_lines ~f:(fun r_line ->
        match String.split r_line ~on:'|' with
        | [fst; snd] -> (Int.of_string fst, Int.of_string snd)
        | _ -> failwith "invalid rule parse" )

  let parse_updates u_lines =
    List.map u_lines ~f:(fun u_line ->
        String.split u_line ~on:',' |> List.map ~f:Int.of_string )

  (* Parse the input to type t, invoked for both parts *)
  let parse _inputs =
    let lines = String.split_lines _inputs in
    let r_lines, u_lines =
      List.split_while lines ~f:(fun s -> not @@ String.is_empty s)
    in
    let u_lines = List.drop u_lines 1 in
    (parse_rules r_lines, parse_updates u_lines)

  let check_update rules update =
    let check_pair_valid a b =
      not
      @@ List.exists rules ~f:(fun (ar, br) ->
             Int.equal a br && Int.equal b ar )
    in
    let rec go before cur middle after =
      match after with
      | [] -> (true, middle)
      | _ ->
          let valid =
            List.fold after ~init:true ~f:(fun v n ->
                v && check_pair_valid cur n )
          in
          if not valid then (false, None)
          else
            go (cur :: before) (List.hd_exn after)
              ( if
                  Option.is_none middle
                  && Int.equal (List.length before) (List.length after)
                then Some cur
                else middle )
              (List.tl_exn after)
    in
    go [] (List.hd_exn update) None (List.tl_exn update)

  (* Run part 1 with parsed inputs *)
  let part1 (rules, updates) =
    Stdlib.print_int
    @@ List.fold updates ~init:0 ~f:(fun acc u ->
           match check_update rules u with
           | true, Some middle -> acc + middle
           | _, _ -> acc ) ;
    Stdlib.print_char ' '

  (* Run part 2 with parsed inputs *)
  let part2 (rules, updates) =
    let compare_updates a b =
      if List.exists rules ~f:(fun (c, d) -> Int.equal a c && Int.equal b d)
      then -1
      else if
        List.exists rules ~f:(fun (c, d) -> Int.equal a d && Int.equal b c)
      then 1
      else 0
    and middle_element lst =
      let rec go before cur after =
        match after with
        | [] -> failwith "even list??"
        | _ ->
            if Int.equal (List.length before) (List.length after) then cur
            else go (cur :: before) (List.hd_exn after) (List.tl_exn after)
      in
      go [] (List.hd_exn lst) (List.tl_exn lst)
    in
    List.filter updates ~f:(fun u ->
        not @@ List.is_sorted u ~compare:compare_updates )
    |> List.map ~f:(fun u -> List.sort u ~compare:compare_updates)
    |> List.map ~f:middle_element
    |> List.fold ~init:0 ~f:Int.( + )
    |> Stdlib.print_int
end

include M
include Day.Make (M)

(* Example input *)
let example =
  "47|53\n\
   97|13\n\
   97|61\n\
   97|47\n\
   75|29\n\
   61|13\n\
   75|53\n\
   29|13\n\
   97|29\n\
   53|29\n\
   61|53\n\
   97|53\n\
   61|29\n\
   47|13\n\
   75|47\n\
   97|75\n\
   47|61\n\
   75|61\n\
   47|29\n\
   75|13\n\
   53|13\n\n\
   75,47,61,53,29\n\
   97,61,53,29,13\n\
   75,29,13\n\
   75,97,47,61,53\n\
   61,13,29\n\
   97,13,75,29,47"

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {| 143 123|}]
