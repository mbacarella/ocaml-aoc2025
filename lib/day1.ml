open! Core

module Dial = struct
  type t = { pos : int } [@@deriving fields]

  let init () = { pos = 50 }
  let is_zero t = t.pos = 0

  let left t turn =
    assert (turn > 0);
    let pos =
      let new_pos = t.pos - turn in
      let remain = new_pos mod 100 in
      if remain < 0 then remain + 100 else remain
    in
    { pos }
  ;;

  let right t turn =
    assert (turn > 0);
    { pos = (t.pos + turn) mod 100 }
  ;;

  let left_with_cross_count t turn =
    let new_t = left t turn in
    let crossed_zero = if new_t.pos > t.pos && not (is_zero t) then 1 else 0 in
    let additional_crosses = turn / 100 in
    new_t, crossed_zero + additional_crosses + if is_zero new_t then 1 else 0
  ;;

  let right_with_cross_count t turn =
    let new_t = right t turn in
    let crossed_zero = if new_t.pos < t.pos then 1 else 0 in
    let additional_crosses = turn / 100 in
    new_t, crossed_zero + additional_crosses
  ;;
end

module Decoder = struct
  module Instr = struct
    type t =
      [ `Left of int
      | `Right of int
      ]
    [@@deriving sexp]

    let decode_int = Int.of_string

    let of_line s =
      let len = String.length s in
      if len < 2 then failwithf "String '%s' too short: < 2" s ();
      match s.[0] with
      | 'L' -> `Left (decode_int (String.sub s ~pos:1 ~len:(pred len)))
      | 'R' -> `Right (decode_int (String.sub s ~pos:1 ~len:(pred len)))
      | _ -> failwithf "Unknown line: %s" s ()
    ;;
  end

  type t = Instr.t list [@@deriving sexp]

  let of_lines lines = lines |> List.map ~f:Instr.of_line

  let process_lines_v1 lines =
    let init_state = Dial.init (), 0 in
    let final_state =
      lines
      |> of_lines
      |> List.fold_left ~init:init_state ~f:(fun (dial, times_on_zero) instr ->
        let dial =
          match instr with
          | `Left mag -> Dial.left dial mag
          | `Right mag -> Dial.right dial mag
        in
        let times_on_zero =
          if Dial.is_zero dial then succ times_on_zero else times_on_zero
        in
        dial, times_on_zero)
    in
    let times_on_zero = snd final_state in
    times_on_zero
  ;;

  let process_lines_v2_slow_and_dumb lines =
    let init_state = Dial.init (), 0 in
    let final_state =
      lines
      |> of_lines
      |> List.fold_left ~init:init_state ~f:(fun (dial, times_on_zero) instr ->
        (* This is slow and dumb but it works and is simpler to understand IMO. *)
        let rec compute (dial, times_on_zero) instr =
          match instr with
          | `Left 0 | `Right 0 -> dial, times_on_zero
          | `Left mag ->
            let dial = Dial.left dial 1 in
            let times_on_zero =
              if Dial.is_zero dial then succ times_on_zero else times_on_zero
            in
            compute (dial, times_on_zero) (`Left (pred mag))
          | `Right mag ->
            let dial = Dial.right dial 1 in
            let times_on_zero =
              if Dial.is_zero dial then succ times_on_zero else times_on_zero
            in
            compute (dial, times_on_zero) (`Right (pred mag))
        in
        compute (dial, times_on_zero) instr)
    in
    let times_on_zero = snd final_state in
    times_on_zero
  ;;

  let process_lines_v2 ?(debug = false) lines =
    let init_state = Dial.init (), 0 in
    let final_state =
      lines
      |> of_lines
      |> List.fold_left ~init:init_state ~f:(fun (dial, times_on_zero) instr ->
        let dial', cross_count =
          match instr with
          | `Left turns -> Dial.left_with_cross_count dial turns
          | `Right turns -> Dial.right_with_cross_count dial turns
        in
        if debug
        then (
          match instr with
          | `Left turns ->
            printf
              "L%d %d -> %d (zeroes: %d)\n"
              turns
              dial.pos
              dial'.pos
              (times_on_zero + cross_count)
          | `Right turns ->
            printf
              "R%d %d -> %d (zeroes: %d)\n"
              turns
              dial.pos
              dial'.pos
              (times_on_zero + cross_count));
        dial', times_on_zero + cross_count)
    in
    let times_on_zero = snd final_state in
    times_on_zero
  ;;

  let%test_module "test module" =
    (module struct
      let example_blob =
        {|L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
|}
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
          \ ((Left 68) (Left 30) (Right 48) (Left 5) (Right 60) (Left 55) (Left 1)\n\
          \  (Left 99) (Right 14) (Left 82))\n\
          \ "]
      ;;

      let%expect_test "basic decode all v1" =
        example_lines |> process_lines_v1 |> printf "%d\n";
        [%expect "3"]
      ;;

      let%expect_test "basic decode all v2" =
        example_lines |> process_lines_v2 ~debug:true |> printf "%d\n";
        [%expect
          " \n\
          \ L68 50 -> 82 (zeroes: 1)\n\
          \ L30 82 -> 52 (zeroes: 1)\n\
          \ R48 52 -> 0 (zeroes: 2)\n\
          \ L5 0 -> 95 (zeroes: 2)\n\
          \ R60 95 -> 55 (zeroes: 3)\n\
          \ L55 55 -> 0 (zeroes: 4)\n\
          \ L1 0 -> 99 (zeroes: 4)\n\
          \ L99 99 -> 0 (zeroes: 5)\n\
          \ R14 0 -> 14 (zeroes: 5)\n\
          \ L82 14 -> 32 (zeroes: 6)\n\
          \ 6\n\
          \ "]
      ;;
    end)
  ;;

  (** [process_file file_name] processes the file of instructions, moves the dial accordingly, and returns the count
      of how many times it landed on zero. *)
  let process_file_v1 file_name = file_name |> In_channel.read_lines |> process_lines_v1

  (** [process_file_v2 file_name] processes the file of instructions, moves the dial accordingly, and returns the count
      of how many times it landed on zero. *)
  let process_file_v2 file_name = file_name |> In_channel.read_lines |> process_lines_v2
end

let main ~input_file ~v2 () =
  let times_on_zero =
    match v2 with
    | true -> Decoder.process_file_v2 input_file
    | false -> Decoder.process_file_v1 input_file
  in
  printf "%d\n" times_on_zero
;;

let cmd =
  Command.basic
    ~summary:"day 1"
    (let%map_open.Command input_file = anon ("input" %: string)
     and v2 = flag "v2" no_arg ~doc:"step 1 or 2" in
     fun () -> main ~input_file ~v2 ())
;;
