let cmd () =
  let open Aoc2025 in
  Command.group
    ~summary:"Advent of Code 2025"
    [ "day0", Day0.cmd
    ; "day1", Day1.cmd
    ; "day2", Day2.cmd
    ; "day3", Day3.cmd
    ; "day4", Day4.cmd
    ; "day5", Day5.cmd
    ; "day6", Day6.cmd
    ; "day7", Day7.cmd
    ; "day8", Day8.cmd
    ; "day9", Day9.cmd
    ; "day10", Day10.cmd
    ; "day11", Day11.cmd
    ]
;;

let () = Command_unix.run (cmd ())
