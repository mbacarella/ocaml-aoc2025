open! Core

module Decoder = struct
  module Rolls = struct
    type t = bool Array.t Array.t [@@deriving sexp]

    let of_line s =
      String.to_list s
      |> List.map ~f:(function
        | '.' -> false
        | '@' -> true
        | c -> failwithf "unknown char: %c" c ())
      |> Array.of_list
    ;;

    let of_lines s = s |> List.map ~f:of_line |> Array.of_list

    let projections =
      Memo.unit (fun () ->
        List.cartesian_product [ -1; 0; 1 ] [ -1; 0; 1 ]
        |> List.filter ~f:(function
          | 0, 0 -> false (* punch out the center *)
          | _ -> true))
    ;;

    let get_accessible (t : bool array array) =
      let rows_height = Array.length t in
      Array.foldi t ~init:[] ~f:(fun x acc row ->
        let row_length = Array.length row in
        Array.foldi row ~init:acc ~f:(fun y acc cell ->
          match cell with
          | false -> acc
          | true ->
            let adjacent_rolls =
              projections ()
              |> List.map ~f:(fun (xproj, yproj) -> x + xproj, y + yproj)
              |> List.filter ~f:(fun (xi, yi) ->
                xi >= 0 && yi >= 0 && xi < row_length && yi < rows_height && t.(xi).(yi))
            in
            if List.length adjacent_rolls < 4 then (x, y) :: acc else acc))
    ;;
  end

  let solve_v2 t =
    let rec loop total_rolls_taken =
      match Rolls.get_accessible t with
      | [] -> total_rolls_taken
      | rolls_taken_this_round ->
        List.iter rolls_taken_this_round ~f:(fun (x, y) -> t.(x).(y) <- false);
        loop (total_rolls_taken + List.length rolls_taken_this_round)
    in
    loop 0
  ;;

  let solve_v1 t = Rolls.get_accessible t |> List.length
  let of_blob s = String.split ~on:'\n' s |> Rolls.of_lines

  let example_blob =
    "..@@.@@@@.\n\
     @@@.@.@.@@\n\
     @@@@@.@.@@\n\
     @.@@@@..@.\n\
     @@.@@@@.@@\n\
     .@@@@@@@.@\n\
     .@.@.@.@@@\n\
     @.@@@.@@@@\n\
     .@@@@@@@@.\n\
     @.@.@@@.@."
  ;;

  let%expect_test "dump" =
    example_blob |> of_blob |> Rolls.sexp_of_t |> Sexp.to_string |> print_endline;
    [%expect
      "((false false true true false true true true true false)(true true true false \
       true false true false true true)(true true true true true false true false true \
       true)(true false true true true true false false true false)(true true false true \
       true true true false true true)(false true true true true true true true false \
       true)(false true false true false true false true true true)(true false true true \
       true false true true true true)(false true true true true true true true true \
       false)(true false true false true true true false true false))"]
  ;;

  let%expect_test "example" =
    example_blob |> of_blob |> solve_v1 |> printf "%d\n";
    [%expect "13"]
  ;;

  let%expect_test "example" =
    example_blob |> of_blob |> solve_v2 |> printf "%d\n";
    [%expect "43"]
  ;;

  let process_file ~v2 file_name =
    let t = file_name |> In_channel.read_lines |> Rolls.of_lines in
    if v2 then solve_v2 t else solve_v1 t
  ;;
end

let main ~input_file ~v2 () =
  let result = Decoder.process_file ~v2 input_file in
  printf "%d\n" result
;;

let cmd =
  Command.basic
    ~summary:"day 4"
    (let%map_open.Command input_file = anon ("input" %: string)
     and v2 = flag "v2" no_arg ~doc:"step 1 or 2" in
     fun () -> main ~input_file ~v2 ())
;;
