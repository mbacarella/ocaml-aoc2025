open! Core

module Coord = struct
  type t = int * int [@@deriving sexp]

  let of_line s =
    let x, y = String.strip s |> String.lsplit2_exn ~on:',' in
    Int.(of_string x, of_string y)
  ;;
end

type t = Coord.t list [@@deriving sexp]

let solve_v1 cs =
  List.fold_left cs ~init:0 ~f:(fun max_area (x, y) ->
    List.fold_left cs ~init:max_area ~f:(fun max_area (x', y') ->
      let dx = Int.abs (x - x') in
      let dy = Int.abs (y - y') in
      let area = (dx + 1) * (dy + 1) in
      Int.max area max_area))
;;

module Right_angle_poly = struct
  type t = Coord.t list

  let perimeter = function
    | [] -> assert false
    | hd :: tl as cs ->
      (* v0,v1; v1,v2; ... vn,v0 *)
      let edges = List.zip_exn cs (tl @ [ hd ]) in
      List.concat_map edges ~f:(fun (p1, p2) ->
        let x1, y1 = p1 in
        let x2, y2 = p2 in
        let dx = Int.(x2 - x1) in
        let dy = Int.(y2 - y1) in
        assert (dx = 0 || dy = 0);
        let sx = if dx > 0 then 1 else if dx < 0 then -1 else 0 in
        let sy = if dy > 0 then 1 else if dy < 0 then -1 else 0 in
        let steps = Int.(max (abs dx) (abs dy)) in
        List.range 0 steps ~start:`inclusive ~stop:`exclusive
        |> List.map ~f:(fun i -> x1 + (i * sx), y1 + (i * sy)))
  ;;
end

let solve_v2 cs =
  let perimeter = Right_angle_poly.perimeter cs in
  let rect_is_inside_perimeter x1 y1 x2 y2 =
    let xmin, xmax = Int.min x1 x2, Int.max x1 x2 in
    let ymin, ymax = Int.min y1 y2, Int.max y1 y2 in
    not
      (List.exists perimeter ~f:(fun (x, y) ->
         x > xmin && x < xmax && y > ymin && y < ymax))
  in
  List.fold_left cs ~init:0 ~f:(fun areamax (x1, y1) ->
    List.fold_left cs ~init:areamax ~f:(fun areamax (x2, y2) ->
      let dx = Int.abs (x1 - x2) in
      let dy = Int.abs (y1 - y2) in
      let area = (dx + 1) * (dy + 1) in
      if Int.(area > areamax) && rect_is_inside_perimeter x1 y1 x2 y2
      then area
      else areamax))
;;

let of_lines lst =
  List.filter lst ~f:(String.( <> ) "") |> List.map ~f:Coord.of_line
;;

let example_blob = "7,1\n11,1\n11,7\n9,7\n9,5\n2,5\n2,3\n7,3\n"
let of_blob b = String.split ~on:'\n' b |> of_lines

let%expect_test "solve v1" =
  let grid = example_blob |> of_blob in
  solve_v1 grid |> printf "%d\n";
  [%expect "50"]
;;

let%expect_test "solve v2" =
  let grid = example_blob |> of_blob in
  solve_v2 grid |> printf "%d\n";
  [%expect "24"]
;;

let%expect_test "dump" =
  example_blob |> of_blob |> sexp_of_t |> Sexp.to_string |> print_endline;
  [%expect "((7 1)(11 1)(11 7)(9 7)(9 5)(2 5)(2 3)(7 3))"]
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
    ~summary:"day 9"
    (let%map_open.Command input_file = anon ("input" %: string)
     and v2 = flag "v2" no_arg ~doc:"step 1 or 2" in
     fun () -> main ~input_file ~v2 ())
;;
