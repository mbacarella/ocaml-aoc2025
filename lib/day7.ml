open! Core

module Decoder = struct
  module Diagram = struct
    module Cell_state = struct
      type t =
        | Empty
        | Start
        | Splitter
        | Tachyon of int
      [@@deriving sexp]

      let to_string = function
        | Start -> " S"
        | Empty -> " ."
        | Splitter -> " ^"
        | Tachyon x -> sprintf "%2d" x
      ;;

      let routes_or_zero = function
        | Tachyon x -> x
        | _ -> 0
      ;;
    end

    type t = Cell_state.t Array.t Array.t [@@deriving sexp]

    let to_string t =
      let b = Buffer.create 100 in
      for i = 0 to pred (Array.length t) do
        for j = 0 to pred (Array.length t.(i)) do
          Buffer.add_string b (Cell_state.to_string t.(i).(j))
        done;
        Buffer.add_char b '\n'
      done;
      Buffer.contents b
    ;;

    let of_lines lst =
      lst
      |> List.filter ~f:(String.( <> ) "")
      |> List.map ~f:(fun s ->
        String.to_array s
        |> Array.map ~f:(function
          | 'S' -> Cell_state.Start
          | '.' -> Cell_state.Empty
          | '^' -> Cell_state.Splitter
          | c -> failwithf "unexpected cell char: %c" c ()))
      |> Array.of_list
    ;;

    let of_blob s = s |> String.split ~on:'\n' |> of_lines
  end

  let solve_gen t =
    let row_length = Array.length t.(0) in
    let num_splits = ref 0 in
    for i = 1 to Array.length t - 1 do
      let row = t.(i) in
      let prev_row = t.(i - 1) in
      for j = 0 to row_length - 1 do
        let open Diagram.Cell_state in
        match row.(j), prev_row.(j) with
        | Empty, Start -> row.(j) <- Tachyon 1
        | Empty, Tachyon routes -> row.(j) <- Tachyon routes
        | Empty, (Splitter | Empty) -> ()
        | Start, _ -> assert false
        | Tachyon _, _ -> ()
        | Splitter, Empty -> ()
        | Splitter, (Splitter | Start) -> assert false
        | Splitter, Tachyon routes ->
          incr num_splits;
          let left = pred j in
          let right = succ j in
          if left >= 0
          then (
            let above = routes_or_zero prev_row.(left) in
            match row.(left) with
            | Empty -> row.(left) <- Tachyon (routes + above)
            | Tachyon routes' -> row.(left) <- Tachyon (routes + routes')
            | _ -> assert false);
          if right <= row_length
          then (
            let above = routes_or_zero prev_row.(right) in
            match row.(right) with
            | Empty -> row.(right) <- Tachyon (routes + above)
            | Tachyon routes' -> row.(right) <- Tachyon (routes + routes' + above)
            | _ -> assert false)
      done
    done;
    !num_splits, t
  ;;

  let example_blob =
    ".......S.......\n\
     ...............\n\
     .......^.......\n\
     ...............\n\
     ......^.^......\n\
     ...............\n\
     .....^.^.^.....\n\
     ...............\n\
     ....^.^...^....\n\
     ...............\n\
     ...^.^...^.^...\n\
     ...............\n\
     ..^...^.....^..\n\
     ...............\n\
     .^.^.^.^.^...^.\n\
     ..............."
  ;;

  let solve_v1 t =
    let num_splits, t' = solve_gen t in
    t' |> Diagram.to_string |> print_endline;
    num_splits
  ;;

  let solve_v2 t =
    let _, _t = solve_gen t in
    let last_row = Array.last t in
    Array.fold last_row ~init:0 ~f:(fun acc cell ->
      let open Diagram.Cell_state in
      match cell with
      | Tachyon routes -> acc + routes
      | Empty -> acc
      | Splitter | Start -> assert false)
  ;;

  let%expect_test "example dump" =
    example_blob |> Diagram.of_blob |> Diagram.to_string |> print_endline;
    [%expect
      " \n\
      \ .......S.......\n\
      \ ...............\n\
      \ .......^.......\n\
      \ ...............\n\
      \ ......^.^......\n\
      \ ...............\n\
      \ .....^.^.^.....\n\
      \ ...............\n\
      \ ....^.^...^....\n\
      \ ...............\n\
      \ ...^.^...^.^...\n\
      \ ...............\n\
      \ ..^...^.....^..\n\
      \ ...............\n\
      \ .^.^.^.^.^...^.\n\
      \ ...............\n\
      \ "]
  ;;

  let%expect_test "example solve" =
    example_blob |> Diagram.of_blob |> solve_v1 |> printf "%d\n";
    [%expect
      " \n\
      \ .......S.......\n\
      \ .......|.......\n\
      \ ......|^|......\n\
      \ ......|.|......\n\
      \ .....|^|^|.....\n\
      \ .....|.|.|.....\n\
      \ ....|^|^|^|....\n\
      \ ....|.|.|.|....\n\
      \ ...|^|^|||^|...\n\
      \ ...|.|.|||.|...\n\
      \ ..|^|^|||^|^|..\n\
      \ ..|.|.|||.|.|..\n\
      \ .|^|||^||.||^|.\n\
      \ .|.|||.||.||.|.\n\
      \ |^|^|^|^|^|||^|\n\
      \ |.|.|.|.|.|||.|\n\n\
      \ 21\n\
      \ "]
  ;;

  let%expect_test "example solve v2" =
    example_blob |> Diagram.of_blob |> solve_v2 |> printf "%d\n";
    [%expect ""]
  ;;

  let process_file ~v2 file_name =
    let t = file_name |> In_channel.read_all |> Diagram.of_blob in
    if v2 then solve_v2 t else solve_v1 t
  ;;
end

let main ~input_file ~v2 () =
  let result = Decoder.process_file ~v2 input_file in
  printf "%d\n" result
;;

let cmd =
  Command.basic
    ~summary:"day 5"
    (let%map_open.Command input_file = anon ("input" %: string)
     and v2 = flag "v2" no_arg ~doc:"step 1 or 2" in
     fun () -> main ~input_file ~v2 ())
;;
