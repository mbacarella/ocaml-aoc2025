open! Core

module Graph = struct
  type neighbors = string list [@@deriving sexp]
  type t = neighbors String.Table.t [@@deriving sexp]

  let of_line s =
    let node, neighbors = String.lsplit2_exn ~on:':' s in
    let neighbors =
      String.split ~on:' ' neighbors
      |> List.map ~f:String.strip
      |> List.filter ~f:(String.( <> ) "")
    in
    String.strip node, neighbors
  ;;

  let of_lines lst =
    List.concat_map lst ~f:(fun line ->
      let node, neighbors = of_line line in
      List.map neighbors ~f:(fun n -> node, n))
    |> String.Table.of_alist_multi
  ;;
end

let solve_gen start t =
  let memo = Hashtbl.Poly.create () in
  let rec count node ~dac_seen ~fft_seen =
    let dac_seen = dac_seen || String.(node = "dac") in
    let fft_seen = fft_seen || String.(node = "fft") in
    let key = node, dac_seen, fft_seen in
    Hashtbl.find_or_add memo key ~default:(fun () ->
      if String.(node = "out")
      then Bool.to_int (dac_seen && fft_seen)
      else (
        let neighbors = Hashtbl.find_exn t node in
        List.fold neighbors ~init:0 ~f:(fun acc neighbor ->
          acc + count neighbor ~dac_seen ~fft_seen)))
  in
  function
  | `p2 -> count start ~dac_seen:false ~fft_seen:false
  | `p1 -> count start ~dac_seen:true ~fft_seen:true
;;

let solve_v1 t = solve_gen "you" t `p1
let solve_v2 t = solve_gen "svr" t `p2

let example_blob =
  "aaa: you hhh\n\
   you: bbb ccc\n\
   bbb: ddd eee\n\
   ccc: ddd eee fff\n\
   ddd: ggg\n\
   eee: out\n\
   fff: out\n\
   ggg: out\n\
   hhh: ccc fff iii\n\
   iii: out"
;;

let of_lines lst = List.filter lst ~f:(String.( <> ) "") |> Graph.of_lines
let of_blob b = String.split ~on:'\n' b |> of_lines

let%expect_test "solve v1" =
  let g = example_blob |> of_blob in
  solve_v1 g |> printf "%d\n";
  [%expect "5"]
;;

let example_blob2 =
  "svr: aaa bbb\n\
   aaa: fft\n\
   fft: ccc\n\
   bbb: tty\n\
   tty: ccc\n\
   ccc: ddd eee\n\
   ddd: hub\n\
   hub: fff\n\
   eee: dac\n\
   dac: fff\n\
   fff: ggg hhh\n\
   ggg: out\n\
   hhh: out"
;;

let%expect_test "solve v2" =
  let g = example_blob2 |> of_blob in
  solve_v2 g |> printf "%d\n";
  [%expect "2"]
;;

let%expect_test "dump" =
  example_blob |> of_blob |> Graph.sexp_of_t |> Sexp.to_string |> print_endline;
  [%expect
    "((aaa(hhh you))(bbb(eee ddd))(ccc(fff eee \
     ddd))(ddd(ggg))(eee(out))(fff(out))(ggg(out))(hhh(iii fff \
     ccc))(iii(out))(you(ccc bbb)))"]
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
    ~summary:"day 11"
    (let%map_open.Command input_file = anon ("input" %: string)
     and v2 = flag "v2" no_arg ~doc:"step 1 or 2" in
     fun () -> main ~input_file ~v2 ())
;;
