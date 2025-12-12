open! Core

module Present_shape = struct
  type t = bool Array.t Array.t [@@deriving sexp]

  let of_lines lines : t =
    Array.of_list (List.tl_exn lines)
    |> Array.map ~f:(fun row ->
      String.to_array row
      |> Array.map ~f:(function
        | '#' -> true
        | '.' -> false
        | _ -> assert false))
  ;;
end

module Tree_spec = struct
  type t =
    { width : int
    ; height : int
    ; present_counts : int Array.t
    }
  [@@deriving sexp, fields]

  let of_line s =
    match String.split ~on:' ' s with
    | dim :: present_counts ->
      let dim = String.chop_suffix_exn dim ~suffix:":" in
      let width, height = String.lsplit2_exn dim ~on:'x' in
      let present_counts = List.map ~f:Int.of_string present_counts in
      { width = Int.of_string width
      ; height = Int.of_string height
      ; present_counts = Array.of_list present_counts
      }
    | _ -> assert false
  ;;
end

type t = Present_shape.t list * Tree_spec.t list [@@deriving sexp]

let of_lines lst =
  let lst = List.filter lst ~f:(String.( <> ) "") in
  let shapes =
    List.take lst (4 * 6)
    |> List.chunks_of ~length:4
    |> List.map ~f:Present_shape.of_lines
  in
  shapes, List.drop lst (4 * 6) |> List.map ~f:Tree_spec.of_line
;;

let solve_v1 (_shapes, trees) =
  List.fold trees ~init:0 ~f:(fun num_fits tree ->
    let tree_area = tree.Tree_spec.height * tree.width in
    let max_possible_present_area =
      Array.fold tree.present_counts ~init:0 ~f:(fun acc present_count ->
        acc + (present_count * 9))
    in
    num_fits + Bool.to_int (tree_area >= max_possible_present_area))
;;

let solve_v2 _ = 0

let example_blob =
  "0:\n\
   ###\n\
   ##.\n\
   ##.\n\n\
   1:\n\
   ###\n\
   ##.\n\
   .##\n\n\
   2:\n\
   .##\n\
   ###\n\
   ##.\n\n\
   3:\n\
   ##.\n\
   ###\n\
   ##.\n\n\
   4:\n\
   ###\n\
   #..\n\
   ###\n\n\
   5:\n\
   ###\n\
   .#.\n\
   ###\n\n\
   4x4: 0 0 0 0 2 0\n\
   12x5: 1 0 1 0 2 2\n\
   12x5: 1 0 1 0 3 2"
;;

let of_blob b = String.split ~on:'\n' b |> of_lines

let%expect_test "dump" =
  example_blob |> of_blob |> sexp_of_t |> Sexp.to_string |> print_endline;
  [%expect ""]
;;

let%expect_test "solve v1" =
  let g = example_blob |> of_blob in
  solve_v1 g |> printf "%d\n";
  [%expect "0"]
;;

let process_file ~v2 file_name =
  file_name
  |> In_channel.read_lines
  |> of_lines
  |> if v2 then solve_v2 else solve_v1
;;

let main ~input_file ~v2 () =
  let result = process_file ~v2 input_file in
  printf "%d\n" result
;;

let cmd =
  Command.basic
    ~summary:"day 12"
    (let%map_open.Command input_file = anon ("input" %: string)
     and v2 = flag "v2" no_arg ~doc:"step 1 or 2" in
     fun () -> main ~input_file ~v2 ())
;;
