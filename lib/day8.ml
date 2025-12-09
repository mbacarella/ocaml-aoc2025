open! Core

module Box = struct
  type t =
    { x : float
    ; y : float
    ; z : float
    }
  [@@deriving sexp, fields, equal, compare]

  let to_string t = sprintf "(%.0f,%.0f,%.0f)" t.x t.y t.z

  let of_line s =
    match String.split s ~on:',' with
    | [ x; y; z ] ->
      Some { x = Float.of_string x; y = Float.of_string y; z = Float.of_string z }
    | _ -> None
  ;;

  let dist p q =
    sqrt (((p.x -. q.x) ** 2.0) +. ((p.y -. q.y) ** 2.0) +. ((p.z -. q.z) ** 2.0))
  ;;

  let%expect_test "dist origin to (1,1,1)" =
    dist { x = 0.; y = 0.; z = 0. } { x = 1.; y = 1.; z = 1. } |> printf "%f\n";
    [%expect {|1.732051|}]
  ;;

  let%expect_test "dist same point is zero" =
    dist { x = 3.5; y = -2.0; z = 9.25 } { x = 3.5; y = -2.0; z = 9.25 } |> printf "%f\n";
    [%expect {|0.000000|}]
  ;;
end

module Circuit = struct
  type t = Box.t Set.Poly.t [@@deriving sexp, compare]

  let mem t box = Set.mem t box
  let add t box = Set.add t box

  let create p q =
    let set = Set.Poly.empty in
    let set = Set.add (Set.add set p) q in
    set
  ;;

  let equal a b = List.equal Box.equal (Set.to_list a) (Set.to_list b)
end

type circuits = Circuit.t list [@@deriving sexp]
type t = Box.t list [@@deriving sexp]

let of_lines s = s |> List.map ~f:Box.of_line |> List.filter_map ~f:Fn.id

let join_circuits =
  let rec loop circuits =
    let rec aux prefix = function
      | [] -> None
      | c :: rest ->
        let rec aux_more acc = function
          | [] -> aux (c :: prefix) rest
          | d :: ds ->
            if Set.is_empty (Set.inter c d)
            then aux_more (d :: acc) ds
            else (
              let merged = Set.union c d in
              let remaining = List.rev_append prefix (List.rev_append acc ds) in
              Some (merged :: remaining))
        in
        aux_more [] rest
    in
    (* keep applying until no changes *)
    match aux [] circuits with
    | None -> circuits
    | Some circuits' -> loop circuits'
  in
  fun circuits -> loop circuits
;;

let add_circuit circuits p q =
  let already_added =
    List.exists circuits ~f:(fun circuit ->
      Circuit.mem circuit p && Circuit.mem circuit q)
  in
  if already_added
  then circuits
  else (
    let circuits = List.dedup_and_sort ~compare:Circuit.compare circuits in
    let changes = ref false in
    let circuits =
      List.map circuits ~f:(fun circuit ->
        match Circuit.mem circuit p, Circuit.mem circuit q with
        | false, false -> circuit
        | true, false ->
          changes := true;
          Circuit.add circuit q
        | false, true ->
          changes := true;
          Circuit.add circuit p
        | true, true -> assert false)
    in
    Circuit.create p q :: circuits)
;;

let unordered_pairs lst =
  let rec loop acc = function
    | [] | [ _ ] -> List.rev acc
    | hd :: tl ->
      let acc = List.fold_left tl ~init:acc ~f:(fun acc y -> (hd, y) :: acc) in
      loop acc tl
  in
  loop [] lst
;;

let solve_v1 ?(first_n_pairs = 1000) t =
  let box_pairs = unordered_pairs t in
  let sorted_pairs =
    List.map box_pairs ~f:(fun (p, q) -> Box.dist p q, (p, q))
    |> List.sort ~compare:(fun a b -> Float.compare (fst a) (fst b))
  in
  let circuits =
    let first_n_sorted_pairs = List.take sorted_pairs first_n_pairs |> List.map ~f:snd in
    List.fold_left first_n_sorted_pairs ~init:[] ~f:(fun circuits (p, q) ->
      add_circuit circuits p q)
  in
  let joined_circuits = join_circuits circuits in
  let sorted_circuits =
    List.map joined_circuits ~f:(fun circuit -> Set.length circuit, circuit)
    |> List.sort ~compare:(fun a b -> Int.compare (fst b) (fst a))
  in
  List.iteri sorted_circuits ~f:(fun i (_len, circuit) ->
    printf
      "%d: %d %s\n"
      i
      (Set.length circuit)
      (Set.to_list circuit |> List.map ~f:Box.to_string |> String.concat ~sep:"; "));
  let product_of_first_3 =
    List.take sorted_circuits 3
    |> List.fold_left ~init:1 ~f:(fun product (len, _) -> product * len)
  in
  product_of_first_3
;;

let add_or_merge cs p q =
  if List.exists cs ~f:(fun c -> Circuit.mem c p && Circuit.mem c q)
  then cs (* already merged in *)
  else (
    let matches, rest =
      List.partition_tf cs ~f:(fun c -> Circuit.mem c p || Circuit.mem c q)
    in
    let merged = List.fold_left matches ~init:(Circuit.create p q) ~f:Set.union in
    merged :: rest)
;;

let solve_v2 boxes =
  let total = List.length boxes in
  let sorted_pairs =
    unordered_pairs boxes
    |> List.map ~f:(fun (p, q) -> Box.dist p q, (p, q))
    |> List.sort ~compare:(fun (a, _) (b, _) -> Float.compare a b)
  in
  let rec loop circuits = function
    | [] -> assert false
    | (_d, (p, q)) :: tl ->
      (match add_or_merge circuits p q with
       | [ final_circuit ] when Set.length final_circuit = total ->
         Float.to_int (p.Box.x *. q.x)
       | circuits -> loop circuits tl)
  in
  loop [] sorted_pairs
;;

let example_blob =
  "162,817,812\n\
   57,618,57\n\
   906,360,560\n\
   592,479,940\n\
   352,342,300\n\
   466,668,158\n\
   542,29,236\n\
   431,825,988\n\
   739,650,466\n\
   52,470,668\n\
   216,146,977\n\
   819,987,18\n\
   117,168,530\n\
   805,96,715\n\
   346,949,466\n\
   970,615,88\n\
   941,993,340\n\
   862,61,35\n\
   984,92,344\n\
   425,690,689\n"
;;

let of_blob s = String.split ~on:'\n' s |> of_lines

let%expect_test "dump" =
  example_blob |> of_blob |> sexp_of_t |> Sexp.to_string |> print_endline;
  [%expect
    "(((x 162)(y 817)(z 812))((x 57)(y 618)(z 57))((x 906)(y 360)(z 560))((x 592)(y \
     479)(z 940))((x 352)(y 342)(z 300))((x 466)(y 668)(z 158))((x 542)(y 29)(z 236))((x \
     431)(y 825)(z 988))((x 739)(y 650)(z 466))((x 52)(y 470)(z 668))((x 216)(y 146)(z \
     977))((x 819)(y 987)(z 18))((x 117)(y 168)(z 530))((x 805)(y 96)(z 715))((x 346)(y \
     949)(z 466))((x 970)(y 615)(z 88))((x 941)(y 993)(z 340))((x 862)(y 61)(z 35))((x \
     984)(y 92)(z 344))((x 425)(y 690)(z 689)))"]
;;

let%expect_test "example v1" =
  example_blob |> of_blob |> solve_v1 ~first_n_pairs:10 |> printf "%d\n";
  [%expect
    " \n\
    \ 0: 5 (739,650,466); (805,96,715); (862,61,35); (906,360,560); (984,92,344)\n\
    \ 1: 4 (162,817,812); (346,949,466); (425,690,689); (431,825,988)\n\
    \ 2: 2 (52,470,668); (117,168,530)\n\
    \ 3: 2 (819,987,18); (941,993,340)\n\
    \ 40\n\
    \ "]
;;

let%expect_test "example v2" =
  example_blob |> of_blob |> solve_v2 |> printf "%d\n";
  [%expect "25272"]
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
    ~summary:"day 8"
    (let%map_open.Command input_file = anon ("input" %: string)
     and v2 = flag "v2" no_arg ~doc:"step 1 or 2" in
     fun () -> main ~input_file ~v2 ())
;;
