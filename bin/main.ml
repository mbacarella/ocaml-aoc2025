let cmd () =
  let open Aoc2025 in
  Command.group
    ~summary:"Advent of Code 2025"
    [ "day0", Day0.cmd; "day1", Day1.cmd; "day2", Day2.cmd; "day3", Day3.cmd ]
;;

let () = Command_unix.run (cmd ())
