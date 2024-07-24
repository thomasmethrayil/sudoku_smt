# Z3 Sudoku Solver

## To build

You will need OCaml 5.1.0 or later (because `eio` depends on it). For example, using opam run
``` opam switch create 5.2.0``` in the directory you intend to build this solver.

### Dependencies
If you don't already have these packages

``` opam install core stdio eio eio_main z3```

Using dune, run `dune build`

## To run

``` dune exec -- sudoku_smt FILENAME ```

where FILENAME is the path to a file containing sudoku puzzles encoded as follows:

 - You can include multiple sudoku puzzles in this file
 - Each puzzle must be encoded in a single line with the character `-` or `0` denoting empty cells. For example, 
 
 ```000000010400000000020000000000050407008000300001090000300400200050100000000806000``` 
 or

 ```8--------,--36-----,-7--9-2--,-5---7---,----457--,---1---3-,--1----68,--85---1-,-9----4--``` are acceptable. Anything other than unsigned integers or `-` will be discarded.
