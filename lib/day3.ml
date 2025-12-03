open! Core

module Decoder = struct
  module Battery_bank = struct
    type t = int Array.t [@@deriving sexp]

    let of_string s =
      assert (String.length s > 2);
      Array.init (String.length s) ~f:(fun i -> Char.get_digit_exn s.[i])
    ;;

    let find_highest_joltage t =
      let len = Array.length t in
      let rec aux ~pos ~sum ~digits_needed =
        if digits_needed > 0
        then (
          let big_digit_index = ref pos in
          for i = pos to pred (len - pred digits_needed) do
            if t.(i) > t.(!big_digit_index) then big_digit_index := i
          done;
          aux
            ~pos:(succ !big_digit_index)
            ~sum:((sum * 10) + t.(!big_digit_index))
            ~digits_needed:(pred digits_needed))
        else sum
      in
      fun ~digits_needed -> aux ~pos:0 ~sum:0 ~digits_needed
    ;;

    let%test_module "test module v1" =
      (module struct
        let test s =
          let t = of_string s in
          let j = find_highest_joltage t ~digits_needed:2 in
          printf "%d\n" j
        ;;

        let%expect_test "987654321111111" =
          test "987654321111111";
          [%expect "98"]
        ;;

        let%expect_test "811111111111119" =
          test "811111111111119";
          [%expect "89"]
        ;;

        let%expect_test "234234234234278" =
          test "234234234234278";
          [%expect "78"]
        ;;

        let%expect_test "818181911112111" =
          test "818181911112111";
          [%expect "92"]
        ;;
      end)
    ;;

    let%test_module "test module v2" =
      (module struct
        let test s =
          let t = of_string s in
          let j = find_highest_joltage t ~digits_needed:12 in
          printf "%d\n" j
        ;;

        let%expect_test "987654321111111" =
          test "987654321111111";
          [%expect "987654321111"]
        ;;

        let%expect_test "811111111111119" =
          test "811111111111119";
          [%expect "811111111119"]
        ;;

        let%expect_test "234234234234278" =
          test "234234234234278";
          [%expect "434234234278"]
        ;;

        let%expect_test "818181911112111" =
          test "818181911112111";
          [%expect "888911112111"]
        ;;
      end)
    ;;
  end

  type t = Battery_bank.t list [@@deriving sexp]

  let of_lines = List.map ~f:Battery_bank.of_string

  let process_lines ~v2 lines =
    let banks = of_lines lines in
    let f =
      if v2
      then Battery_bank.find_highest_joltage ~digits_needed:12
      else Battery_bank.find_highest_joltage ~digits_needed:2
    in
    let sum_of_highest_joltages =
      List.fold_left banks ~init:0 ~f:(fun acc bank -> acc + f bank)
    in
    sum_of_highest_joltages
  ;;

  let%test_module "test module" =
    (module struct
      let example_blob =
        "987654321111111\n811111111111119\n234234234234278\n818181911112111"
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
          \ ((9 8 7 6 5 4 3 2 1 1 1 1 1 1 1) (8 1 1 1 1 1 1 1 1 1 1 1 1 1 9)\n\
          \  (2 3 4 2 3 4 2 3 4 2 3 4 2 7 8) (8 1 8 1 8 1 9 1 1 1 1 2 1 1 1))\n\
          \ "]
      ;;

      let%expect_test "basic decode all v1" =
        example_lines |> process_lines ~v2:false |> printf "%d\n";
        [%expect {| 357 |}]
      ;;

      let%expect_test "basic decode all v2" =
        example_lines |> process_lines ~v2:true |> printf "%d\n";
        [%expect "3121910778619"]
      ;;
    end)
  ;;

  (** [process_file file_name] processes the file of instructions, moves the dial accordingly, and returns the count
      of how many times it landed on zero. *)
  let process_file ~v2 file_name = file_name |> In_channel.read_lines |> process_lines ~v2
end

let main ~input_file ~v2 () =
  let sums = Decoder.process_file ~v2 input_file in
  printf "%d\n" sums
;;

let cmd =
  Command.basic
    ~summary:"day 3"
    (let%map_open.Command input_file = anon ("input" %: string)
     and v2 = flag "v2" no_arg ~doc:"step 1 or 2" in
     fun () -> main ~input_file ~v2 ())
;;
