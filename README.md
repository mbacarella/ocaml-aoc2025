Advent of Code 2025
==============

in OCaml
------

I thought I'd do [AOC 2025](https://adventofcode.com/2025) in OCaml this year. 

On social media I tend to see these posted by people using AOC as an impetus to
learn a new language.

This is a little different, I've been writing OCaml professionally since 2008,
so consider this a more experienced take.

I'm using the Jane Street ecosystem since 
* I'm most familiar with it, and
* it helps fast-forward past a lot of struggle points people have with the stock OCaml standard library

Verify!
---

```shell
% dune build @runtest

% dune exec -- bin/day1.exe inputs/input1.txt -v2
```

