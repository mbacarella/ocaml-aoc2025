open! Core

module Decoder = struct
  module Range = struct
    type t =
      { start : int
      ; stop : int
      }
    [@@deriving sexp, compare, equal]

    let to_string t = sprintf "%d-%d" t.start t.stop

    let of_line s =
      let start, stop = String.lsplit2_exn s ~on:'-' in
      { start = Int.of_string start; stop = Int.of_string stop }
    ;;
  end

  type t =
    { fresh_ranges : Range.t list
    ; all_ingredients : int list
    }
  [@@deriving sexp]

  let of_lines lst =
    let pred = String.( <> ) "" in
    let fresh_ranges = List.take_while lst ~f:pred |> List.map ~f:Range.of_line in
    let all_ingredients =
      lst |> List.drop_while ~f:pred |> List.tl_exn |> List.map ~f:Int.of_string
    in
    { fresh_ranges; all_ingredients }
  ;;

  let solve_v1 t =
    let fresh_ingredients =
      List.fold_left t.all_ingredients ~init:[] ~f:(fun fresh_ingredients ingredient ->
        if
          List.exists t.fresh_ranges ~f:(fun range ->
            ingredient >= range.start && ingredient <= range.stop)
        then ingredient :: fresh_ingredients
        else fresh_ingredients)
    in
    List.length fresh_ingredients
  ;;

  let coalesce_one (orig_lst : Range.t list) =
    let rec aux accum work_lst =
      match work_lst with
      | [] -> accum
      | hd :: tl ->
        (match
           List.find tl ~f:(fun x ->
             hd.Range.start <= x.start && hd.stop >= x.start && hd.stop <= x.stop)
         with
         | None -> aux (hd :: accum) tl
         | Some x ->
           printf "coal %s with %s\n" (Range.to_string hd) (Range.to_string x);
           let new_hd = { Range.start = hd.start; stop = x.stop } in
           printf "new fragment %s\n" (Range.to_string new_hd);
           let new_work_lst =
             List.filter work_lst ~f:(fun x' ->
               (not (Range.equal x x')) && not (Range.equal hd x'))
           in
           aux (new_hd :: accum) new_work_lst)
    in
    aux [] (List.sort ~compare:Range.compare orig_lst)
  ;;

  let coalesce_all lst =
    let rec loop lst =
      let new_lst = coalesce_one lst in
      printf
        "new list after one pass: %s\n"
        (new_lst |> List.map ~f:Range.to_string |> String.concat ~sep:" ");
      printf "%d vs %d\n" (List.length new_lst) (List.length lst);
      if List.length new_lst = List.length lst then new_lst else loop new_lst
    in
    loop lst
  ;;

  let solve_v2 t =
    let final_lst = coalesce_all t.fresh_ranges in
    printf
      "final list: %s\n"
      (final_lst
       |> List.sort ~compare:Range.compare
       |> List.map ~f:Range.to_string
       |> String.concat ~sep:" ");
    List.fold_left final_lst ~init:0 ~f:(fun fresh_count range ->
      fresh_count + (succ range.stop - range.start))
  ;;

  let of_blob s = s |> String.split ~on:'\n' |> of_lines
  let example_blob = "3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32"

  let%expect_test "dump" =
    example_blob |> of_blob |> sexp_of_t |> Sexp.to_string |> print_endline;
    [%expect
      "((fresh_ranges(((start 3)(stop 5))((start 10)(stop 14))((start 16)(stop 20))((start 12)(stop 18))))(all_ingredients(1 5 8 11 17 32)))"]
  ;;

  let%expect_test "example" =
    example_blob |> of_blob |> solve_v1 |> printf "%d\n";
    [%expect "3"]
  ;;

  let%expect_test "example2" =
    example_blob |> of_blob |> solve_v2 |> printf "%d\n";
    [%expect " 
 coal 10-14 with 12-18
 new fragment 10-18
 new list after one pass: 16-20 10-18 3-5
 3 vs 4
 coal 10-18 with 16-20
 new fragment 10-20
 new list after one pass: 10-20 3-5
 2 vs 3
 new list after one pass: 10-20 3-5
 2 vs 2
 final list: 3-5 10-20
 14
 "]
  ;;

  let process_file ~v2 file_name =
    let t = file_name |> In_channel.read_lines |> of_lines in
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
