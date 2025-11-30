open! Core

let main () = printf "hello, world\n"

let cmd =
  Command.basic
    ~summary:"AOC day 1"
    (let%map_open.Command () = return () in
     fun () -> main ())
;;
