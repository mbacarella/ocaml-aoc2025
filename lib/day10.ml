open! Core

let bitmaskstr n =
  if n = 0
  then "0"
  else (
    let k = Int.floor_log2 n in
    String.init (k + 1) ~f:(fun i ->
      if n land (1 lsl (k - i)) <> 0 then '1' else '0'))
;;

module Spec = struct
  type t =
    { indicator : int
    ; toggles : int list
    }
  [@@deriving sexp]

  let to_bit_strings t =
    sprintf
      "%s: %s"
      (bitmaskstr t.indicator)
      (List.map t.toggles ~f:bitmaskstr |> String.concat ~sep:",")
  ;;

  let indicator_mask s =
    String.to_list s
    |> List.foldi ~init:0 ~f:(fun i acc c ->
      match c with
      | '#' -> acc lor (1 lsl i)
      | '.' -> acc
      | _ -> assert false)
  ;;

  let toggle_masks s =
    String.strip s
    |> String.split ~on:','
    |> List.fold ~init:0 ~f:(fun acc x ->
      let i = Int.of_string (String.strip x) in
      acc lor (1 lsl i))
  ;;

  let of_line line =
    let line, _ =
      (* drop the joltage {…} part for now *)
      match String.lsplit2 line ~on:'{' with
      | Some (a, _) -> a, ()
      | None -> line, ()
    in
    let lb = String.index_exn line '[' in
    let rb = String.index_exn line ']' in
    let indicator =
      String.sub line ~pos:(lb + 1) ~len:(rb - lb - 1) |> indicator_mask
    in
    let after =
      String.sub line ~pos:(rb + 1) ~len:(String.length line - rb - 1)
    in
    let toggles =
      String.split ~on:'(' after
      |> List.tl
      |> Option.value ~default:[]
      |> List.map ~f:(fun s ->
        let inside, _ = String.lsplit2_exn s ~on:')' in
        toggle_masks inside)
    in
    { indicator; toggles }
  ;;
end

let yolo_combinations ?(limit = 10_000_000) ~max_comb items =
  let acc = ref [] in
  let count = ref 0 in
  let stop () = !count >= limit in
  let rec gen prefix len_left =
    if not (stop ())
    then
      if len_left = 0
      then (
        incr count;
        acc := List.rev prefix :: !acc)
      else List.iter items ~f:(fun x -> gen (x :: prefix) (len_left - 1))
  in
  for len = 1 to max_comb do
    if not (stop ()) then gen [] len
  done;
  List.rev !acc
;;

type t = Spec.t list [@@deriving sexp]

let solve_v1 specs =
  List.foldi specs ~init:0 ~f:(fun i acc (spec : Spec.t) ->
    let result =
      List.filter_map
        (yolo_combinations ~limit:10_000_000 ~max_comb:8 spec.toggles)
        ~f:(fun toggle_combination ->
          let steps = List.length toggle_combination in
          let final_indicator =
            List.fold_left
              toggle_combination
              ~init:0
              ~f:(fun indicator_state toggle ->
                let indicator_state' = indicator_state lxor toggle in
                indicator_state')
          in
          if spec.indicator = final_indicator then Some steps else None)
      |> List.sort ~compare:Int.compare
      |> List.hd
    in
    match result with
    | None ->
      failwithf "%d: no solution found for %s\n" i (Spec.to_bit_strings spec) ()
    | Some steps ->
      printf "%d: %d steps for %s\n" i steps (Spec.to_bit_strings spec);
      steps + acc)
;;

let solve_v2 _specs = assert false

let of_lines lst =
  List.filter lst ~f:(String.( <> ) "") |> List.map ~f:Spec.of_line
;;

let example_blob =
  "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n\
   [...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}\n\
   [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
;;

let of_blob b = String.split ~on:'\n' b |> of_lines

let%expect_test "solve v1" =
  let specs = example_blob |> of_blob in
  solve_v1 specs |> printf "%d\n";
  [%expect ""]
;;

(*
   let%expect_test "solve v2" =
  let specs = example_blob |> of_blob in
  solve_v2 specs |> printf "%d\n";
  [%expect ""]
;;
*)

let%expect_test "dump" =
  example_blob |> of_blob |> sexp_of_t |> Sexp.to_string |> print_endline;
  [%expect ""]
;;

let process_file ~v2 file_name =
  if v2
  then (
    let t = file_name |> In_channel.read_lines |> of_lines in
    solve_v2 t)
  else (
    let t = file_name |> In_channel.read_lines |> of_lines in
    solve_v1 t)
;;

let main ~input_file ~v2 () =
  let result = process_file ~v2 input_file in
  printf "%d\n" result
;;

let cmd =
  Command.basic
    ~summary:"day 10"
    (let%map_open.Command input_file = anon ("input" %: string)
     and v2 = flag "v2" no_arg ~doc:"step 1 or 2" in
     fun () -> main ~input_file ~v2 ())
;;
