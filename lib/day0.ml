open! Core

let main () =
  printf "This isn't a real AOC day. It's just here to say hello world.\n";
  printf "Hello, world!\n"
;;

let cmd =
  Command.basic
    ~summary:"day 0"
    (let%map_open.Command () = return () in
     fun () -> main ())
;;
