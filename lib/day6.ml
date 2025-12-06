open! Core

module Decoder = struct
  module Worksheet = struct
    type op =
      | Add
      | Mul
    [@@deriving sexp]

    type t = [ `Int of int | `Op of op ] Array.t Array.t [@@deriving sexp]

    let of_line s =
      String.split s ~on:' '
      |> List.filter ~f:(String.( <> ) "")
      |> List.map ~f:(fun num_or_op ->
        match num_or_op with
        | "+" -> `Op Add
        | "*" -> `Op Mul
        | s -> `Int (Int.of_string s))
      |> Array.of_list
    ;;

    let of_lines s = s |> List.map ~f:of_line |> Array.of_list
  end

  let solve_v1 t =
    for _i = 0 to pred (Array.length t) do
      printf "row height: %d\n" (Array.length t.(0))
    done;
    0
  ;;

  let solve_v2 _ = 0
  let example_blob = "123 328  51 64\n45 64  387 23\n6 98  215 314\n*   +   *   +\n"
  let of_blob s = String.split ~on:'\n' s |> Worksheet.of_lines

  let%expect_test "dump" =
    example_blob |> of_blob |> Worksheet.sexp_of_t |> Sexp.to_string |> print_endline;
    [%expect ""]
  ;;

  let%expect_test "example" =
    example_blob |> of_blob |> solve_v1 |> printf "%d\n";
    [%expect ""]
  ;;

  let%expect_test "example" =
    example_blob |> of_blob |> solve_v2 |> printf "%d\n";
    [%expect ""]
  ;;

  let process_file ~v2 file_name =
    let t = file_name |> In_channel.read_lines |> Worksheet.of_lines in
    if v2 then solve_v2 t else solve_v1 t
  ;;
end

let main ~input_file ~v2 () =
  let result = Decoder.process_file ~v2 input_file in
  printf "%d\n" result
;;

let cmd =
  Command.basic
    ~summary:"day 6"
    (let%map_open.Command input_file = anon ("input" %: string)
     and v2 = flag "v2" no_arg ~doc:"step 1 or 2" in
     fun () -> main ~input_file ~v2 ())
;;
