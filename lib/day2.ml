open! Core

module Decoder = struct
  module Range = struct
    type t =
      { start : int
      ; stop : int
      }
    [@@deriving sexp]

    let of_string s : t =
      let a, b = String.lsplit2_exn ~on:'-' s in
      { start = Int.of_string a; stop = Int.of_string b }
    ;;

    let id_has_repeating_digits_v1 s =
      let len = String.length s in
      len mod 2 = 0
      &&
      let a = String.sub s ~pos:0 ~len:(len / 2) in
      let b = String.sub s ~pos:(len / 2) ~len:(len / 2) in
      String.( = ) a b
    ;;

    let id_has_repeating_digits_v2 s =
      let chunk_string length =
        s |> String.to_list |> List.chunks_of ~length |> List.map ~f:String.of_char_list
      in
      let len = String.length s in
      let sizes = List.range ~start:`inclusive ~stop:`inclusive 1 (len / 2) in
      List.exists sizes ~f:(fun size ->
        len mod size = 0
        &&
        match chunk_string size with
        | [] -> assert false
        | hd :: tl -> List.for_all tl ~f:(String.( = ) hd))
    ;;

    let id_has_repeating_digits ~v2 id =
      let f = if v2 then id_has_repeating_digits_v2 else id_has_repeating_digits_v1 in
      f (Int.to_string id)
    ;;

    let ids_with_repeating_digits ~v2 t =
      (*printf "processing range: %d-%d\n" t.start t.stop;*)
      let ids = List.range ~start:`inclusive ~stop:`inclusive t.start t.stop in
      List.fold_left ids ~init:[] ~f:(fun acc id ->
        let has_repeats = id_has_repeating_digits ~v2 id in
        (*printf "checking %d\n" id;*)
        (*if has_repeats then printf "%d has repeats\n" id;*)
        if has_repeats then id :: acc else acc)
    ;;

    let%test_module "test module" =
      (module struct
        let test ?(v2 = false) start stop =
          let t = { start; stop } in
          let ids = ids_with_repeating_digits ~v2 t in
          ids
          |> List.sort ~compare:Int.compare
          |> List.map ~f:Int.to_string
          |> String.concat ~sep:" "
          |> print_endline
        ;;

        let%expect_test "11-22" =
          test 11 22;
          [%expect "11 22"]
        ;;

        let%expect_test "95-115" =
          test 95 115;
          [%expect "99"]
        ;;

        let%expect_test "998-1012" =
          test 998 1012;
          [%expect "1010"]
        ;;

        let%expect_test "95-115 (v2)" =
          test ~v2:true 95 115;
          [%expect "99 111"]
        ;;

        let%expect_test "998-1012" =
          test ~v2:true 998 1012;
          [%expect "999 1010"]
        ;;

        let%expect_test "1188511880-1188511890" =
          test 1188511880 1188511890;
          [%expect "1188511885"]
        ;;

        let%expect_test "1188511880-1188511890" =
          test ~v2:true 1188511880 1188511890;
          [%expect "1188511885"]
        ;;

        let%expect_test "222220-222224" =
          test 222220 222224;
          [%expect "222222"]
        ;;

        let%expect_test "222220-222224" =
          test ~v2:true 222220 222224;
          [%expect "222222"]
        ;;

        let%expect_test "824824821-824824827" =
          test 824824821 824824827;
          [%expect ""]
        ;;

        let%expect_test "824824821-824824827" =
          test ~v2:true 824824821 824824827;
          [%expect "824824824"]
        ;;
      end)
    ;;
  end

  type t = Range.t list [@@deriving sexp]

  let of_lines lines =
    lines |> List.concat_map ~f:(String.split ~on:',') |> List.map ~f:Range.of_string
  ;;

  let process_lines ~v2 lines =
    let ranges = of_lines lines in
    let invalid_ids =
      List.fold_left ranges ~init:[] ~f:(fun acc range ->
        acc @ Range.ids_with_repeating_digits ~v2 range)
    in
    let sum_of_invalid_ids = List.fold_left invalid_ids ~init:0 ~f:( + ) in
    sum_of_invalid_ids
  ;;

  let%test_module "test module" =
    (module struct
      let example_blob =
        "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
      ;;

      let example_lines = String.split_lines example_blob

      let%expect_test "basic decode lines" =
        example_lines |> of_lines |> sexp_of_t |> Sexp.to_string_hum |> print_endline;
        (* N.B. I didn't populate this by hand. I first wrote '[%expect ""]' and then ran [dune build @runtest]
           to have it tell me the real output (and fail because it didn't match). Then I eyeballed it and
           confirmed it looked good, and ran  [dune build @runtest --auto-promote] to accept it and populate it
           into this source file. *)
        [%expect
          " \n\
          \ (((start 11) (stop 22)) ((start 95) (stop 115)) ((start 998) (stop 1012))\n\
          \  ((start 1188511880) (stop 1188511890)) ((start 222220) (stop 222224))\n\
          \  ((start 1698522) (stop 1698528)) ((start 446443) (stop 446449))\n\
          \  ((start 38593856) (stop 38593862)) ((start 565653) (stop 565659))\n\
          \  ((start 824824821) (stop 824824827)) ((start 2121212118) (stop 2121212124)))\n\
          \ "]
      ;;

      let%expect_test "basic decode all v1" =
        example_lines |> process_lines ~v2:false |> printf "%d\n";
        [%expect {| 1227775554 |}]
      ;;

      let%expect_test "basic decode all v2" =
        example_lines |> process_lines ~v2:true |> printf "%d\n";
        [%expect "4174379265"]
      ;;
    end)
  ;;

  (** [process_file file_name] processes the file of instructions, moves the dial accordingly, and returns the count
      of how many times it landed on zero. *)
  let process_file ~v2 file_name = file_name |> In_channel.read_lines |> process_lines ~v2
end

let main ~input_file ~v2 () =
  let result = Decoder.process_file ~v2 input_file in
  printf "%d\n" result
;;

let cmd =
  Command.basic
    ~summary:"day 2"
    (let%map_open.Command input_file = anon ("input" %: string)
     and v2 = flag "v2" no_arg ~doc:"step 1 or 2" in
     fun () -> main ~input_file ~v2 ())
;;
