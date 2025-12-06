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
       6)(Int 98)(Int 215)(Int 314))((Op Mul)(Op Add)(Op Mul)(Op Add)))"]
  ;;

  let%expect_test "example" =
    example_blob |> of_blob |> solve_v1 |> printf "%d\n";
    [%expect "4277556"]
  ;;

  module Worksheet_v2 = struct
    module Col_spec = struct
      type op =
        | Add
        | Mul
      [@@deriving sexp]

      type t =
        { op : op
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
      let col_specs =
        String.fold last_line ~init:(None, []) ~f:(fun (cur, acc) c ->
          let open Col_spec in
          match cur, c with
          | None, '+' -> Some { op = Add; width = 0 }, acc
          | None, '*' -> Some { op = Mul; width = 0 }, acc
          | None, _ -> assert false
          | Some cur, '+' -> Some { op = Add; width = 0 }, cur :: acc
          | Some cur, '*' -> Some { op = Mul; width = 0 }, cur :: acc
          | Some cur, ' ' -> Some { cur with width = succ cur.width }, acc
          | Some _, _ -> assert false)
      in
      match col_specs with
      | None, _ -> assert false
      | Some cur, lst ->
        (* fix last cell *)
        let cur = { cur with width = succ cur.width } in
        Array.of_list (List.rev (cur :: lst))
    ;;

    let of_string data =
      let lines = String.split ~on:'\n' data |> List.filter ~f:(String.( <> ) "") in
      let last_line = List.last_exn lines in
      let col_specs = read_col_specs last_line in
      let row_size_in_chars = String.length last_line + 1 in
      let num_rows = pred (List.length lines) in
      { col_specs; num_rows; row_size_in_chars; data }
    ;;
  end

  let process_column ~op ~width cells =
    let values =
      let rec loop i acc =
        if i = width
        then acc
        else (
          let buf = Buffer.create (List.length cells) in
          List.iter
            ~f:(fun s ->
              let c = s.[i] in
              if Char.(c <> ' ') then Buffer.add_char buf c)
            cells;
          let s = Buffer.contents buf in
          if String.(s = "")
          then loop (i + 1) acc
          else (
            let n = Int.of_string s in
            loop (i + 1) (n :: acc)))
      in
      loop 0 []
    in
    match op with
    | Worksheet_v2.Col_spec.Add -> List.fold_left ~f:( + ) ~init:0 values
    | Mul -> List.fold_left ~f:( * ) ~init:1 values
  ;;

  let%expect_test "example column rightmost" =
    process_column ~op:Worksheet_v2.Col_spec.Add ~width:3 [ "64 "; "23 "; "314" ]
    |> printf "%d\n";
    [%expect ""]
  ;;

  let%expect_test "example column almost rightmost" =
    process_column ~op:Worksheet_v2.Col_spec.Mul ~width:3 [ " 51"; "387"; "215" ]
    |> printf "%d\n";
    [%expect ""]
  ;;

  let%expect_test "example column almost leftmost" =
    process_column ~op:Worksheet_v2.Col_spec.Add ~width:3 [ "328"; "64 "; "98 " ]
    |> printf "%d\n";
    [%expect ""]
  ;;

  let%expect_test "example column leftmost" =
    process_column ~op:Worksheet_v2.Col_spec.Mul ~width:3 [ "123"; " 45"; "  6" ]
    |> printf "%d\n";
    [%expect ""]
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
        printf "%02d:%02d: [%s]\n" col row cell;
        cells := cell :: !cells
      done;
      let column_result =
        process_column ~op:spec.op ~width:spec.width (List.rev !cells)
      in
      printf "col %d result: %d\n" col column_result;
      overall_result := !overall_result + column_result;
      col_offset := !col_offset + spec.width + 1
    done;
    !overall_result
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
      "(((Int 123)(Int 328)(Int 51)(Int 64))((Int 45)(Int 64)(Int 387)(Int 23))((Int \
       6)(Int 98)(Int 215)(Int 314))((Op Mul)(Op Add)(Op Mul)(Op Add)))"]
  ;;

  let%expect_test "example_v2" =
    example_blob_v2 |> of_blob |> solve_v2 |> printf "%d\n";
    [%expect "0"]
  ;;

  let process_file ~v2 file_name =
    if v2
    then (
      let t = file_name |> In_channel.read_all |> Worksheet_v2.of_string in
      solve_v2 t)
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
