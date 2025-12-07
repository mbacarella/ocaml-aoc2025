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
    let num_rows = Array.length t in
    let num_cols = Array.length t.(0) in
    let overall_result = ref 0 in
    for col = 0 to pred num_cols do
      let ops =
        List.range 0 num_rows
        |> List.fold_left ~init:[] ~f:(fun ops row -> t.(row).(col) :: ops)
      in
      let col_result =
        let open Worksheet in
        match ops with
        | `Op Mul :: `Int hd :: tl ->
          List.fold_left tl ~init:hd ~f:(fun product -> function
            | `Int x -> Int.(product * x)
            | `Op _ -> assert false)
        | `Op Add :: `Int hd :: tl ->
          List.fold_left tl ~init:hd ~f:(fun sum -> function
            | `Int x -> Int.(sum + x)
            | `Op _ -> assert false)
        | [] | `Int _ :: _ | `Op Mul :: _ | `Op Add :: _ -> assert false
      in
      overall_result := !overall_result + col_result
    done;
    !overall_result
  ;;

  let example_blob = "123 328  51 64\n 45 64  387 23\n  6 98  215 314\n*   +   *   +  \n"
  let of_blob s = String.split ~on:'\n' s |> Worksheet.of_lines

  let%expect_test "dump" =
    example_blob |> of_blob |> Worksheet.sexp_of_t |> Sexp.to_string |> print_endline;
    [%expect
      "(((Int 123)(Int 328)(Int 51)(Int 64))((Int 45)(Int 64)(Int 387)(Int 23))((Int \
       6)(Int 98)(Int 215)(Int 314))((Op Mul)(Op Add)(Op Mul)(Op Add))())"]
  ;;

  let%expect_test "example" =
    example_blob |> of_blob |> solve_v1 |> printf "%d\n";
    [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)
    (Invalid_argument "index out of bounds")
    Raised by primitive operation at Aoc2025__Day6.Decoder.solve_v1.(fun) in file "lib/day6.ml", line 33, characters 54-67
    Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
    Called from Aoc2025__Day6.Decoder.solve_v1 in file "lib/day6.ml", lines 32-33, characters 8-75
    Called from Aoc2025__Day6.Decoder.(fun) in file "lib/day6.ml", line 64, characters 4-39
    Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28
    |}]
  ;;

  module Worksheet_v2 = struct
    module Col_spec = struct
      type t =
        { op : [ `Add | `Mul ]
        ; width : int
        }
      [@@deriving sexp]
    end

    type t =
      { col_specs : Col_spec.t Array.t
      ; num_rows : int
      ; row_size_in_chars : int
      ; data : string
      }
    [@@deriving sexp]

    let read_col_specs last_line =
      match
        String.fold last_line ~init:(None, []) ~f:(fun (cur, acc) c ->
          let open Col_spec in
          match cur, c with
          | None, '+' -> Some { op = `Add; width = 0 }, acc
          | None, '*' -> Some { op = `Mul; width = 0 }, acc
          | Some cur, '+' -> Some { op = `Add; width = 0 }, cur :: acc
          | Some cur, '*' -> Some { op = `Mul; width = 0 }, cur :: acc
          | Some cur, ' ' -> Some { cur with width = succ cur.width }, acc
          | None, _ | Some _, _ -> assert false)
      with
      | None, _ -> assert false
      | Some cur, lst ->
        let cur = { cur with width = succ cur.width } in
        Array.of_list (List.rev (cur :: lst))
    ;;

    let of_string data =
      let lines = String.split ~on:'\n' data |> List.filter ~f:(String.( <> ) "") in
      let last_line = List.last_exn lines in
      { col_specs = read_col_specs last_line
      ; num_rows = pred (List.length lines)
      ; row_size_in_chars = String.length last_line + 1
      ; data
      }
    ;;
  end

  let process_column ~op ~width cells =
    let cells = Array.of_list cells in
    let num_rows = Array.length cells in
    let numbers = ref [] in
    for i = 0 to pred width do
      let buf = Buffer.create num_rows in
      for j = 0 to pred num_rows do
        let s = cells.(j) in
        let c = s.[i] in
        if Char.(c <> ' ') then Buffer.add_char buf c
      done;
      match Buffer.contents buf with
      | "" -> ()
      | s ->
        let number = Int.of_string s in
        numbers := number :: !numbers
    done;
    let init, f =
      match op with
      | `Add -> 0, ( + )
      | `Mul -> 1, ( * )
    in
    List.fold_left ~f ~init !numbers
  ;;

  let solve_v2 t =
    let open Worksheet_v2 in
    let num_cols = Array.length t.col_specs in
    let col_offset = ref 0 in
    let overall_result = ref 0 in
    for col = 0 to pred num_cols do
      let spec = t.col_specs.(col) in
      let cells = ref [] in
      for row = 0 to pred t.num_rows do
        let cell =
          String.sub
            t.data
            ~pos:((row * t.row_size_in_chars) + !col_offset)
            ~len:spec.width
        in
        cells := cell :: !cells
      done;
      let column_result =
        process_column ~op:spec.op ~width:spec.width (List.rev !cells)
      in
      overall_result := !overall_result + column_result;
      col_offset := !col_offset + spec.width + 1
    done;
    !overall_result
  ;;

  let%expect_test "example column rightmost" =
    process_column ~op:`Add ~width:3 [ "64 "; "23 "; "314" ] |> printf "%d\n";
    [%expect "1058"]
  ;;

  let%expect_test "example column almost rightmost" =
    process_column ~op:`Mul ~width:3 [ " 51"; "387"; "215" ] |> printf "%d\n";
    [%expect "3253600"]
  ;;

  let%expect_test "example column almost leftmost" =
    process_column ~op:`Add ~width:3 [ "328"; "64 "; "98 " ] |> printf "%d\n";
    [%expect "625"]
  ;;

  let%expect_test "example column leftmost" =
    process_column ~op:`Mul ~width:3 [ "123"; " 45"; "  6" ] |> printf "%d\n";
    [%expect "8544"]
  ;;

  let example_blob_v2 =
    "123 328  51 64 \n 45 64  387 23 \n  6 98  215 314\n*   +   *   +  \n"
  ;;

  let of_blob = Worksheet_v2.of_string

  let%expect_test "dump_v2" =
    example_blob_v2
    |> of_blob
    |> Worksheet_v2.sexp_of_t
    |> Sexp.to_string
    |> print_endline;
    [%expect
      "((col_specs(((op Mul)(width 3))((op Add)(width 3))((op Mul)(width 3))((op \
       Add)(width 3))))(num_rows 3)(row_size_in_chars 16)(data\"123 328  51 64 \\n 45 \
       64  387 23 \\n  6 98  215 314\\n*   +   *   +  \\n\"))"]
  ;;

  let%expect_test "example_v2" =
    example_blob_v2 |> of_blob |> solve_v2 |> printf "%d\n";
    [%expect "3263827"]
  ;;

  let solve_v2b grid =
    let num_cols = String.index_exn grid '\n' in
    let max_line_length = succ num_cols in
    let num_rows = String.length grid / max_line_length in
    let col_nums = ref [] in
    List.range (pred num_cols) 0 ~stride:(-1) ~stop:`inclusive
    |> List.fold_left ~init:0 ~f:(fun overall_sum col ->
      let num = ref 0 in
      let c = ref ' ' in
      for row = 0 to num_rows - 1 do
        c := grid.[(row * max_line_length) + col];
        if Char.is_digit !c then num := (!num * 10) + (Char.to_int !c - Char.to_int '0')
      done;
      if !num <> 0 then col_nums := !num :: !col_nums;
      match !c with
      | '+' | '*' ->
        let init, f = if Char.(!c = '+') then 0, ( + ) else 1, ( * ) in
        let col_result = List.fold !col_nums ~init ~f in
        col_nums := [];
        overall_sum + col_result
      | _ -> overall_sum)
  ;;

  let process_file ~v2 file_name =
    if v2
    then (
      (*let t = file_name |> In_channel.read_all |> Worksheet_v2.of_string in*)
      let grid = In_channel.read_all file_name in
      solve_v2b grid)
    else (
      let t = file_name |> In_channel.read_lines |> Worksheet.of_lines in
      solve_v1 t)
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
