#load "Board.fs"
#load "Reader.fs"
#load "Verifier.fs"
#load "Solver.fs"
#load "Utils.fs"

open Puzzle

let b1, c1 = IO.readFile "input_3.txt"

Solver.searchSolutionForBoard c1 b1
// let s1 =
//     [[5;4;4;0;0;7;0]
//      [0;0;7;6;3;4;0]
//      [5;6;2;0;7;0;0]
//      [0;3;0;0;6;6;5]
//      [5;7;0;6;0;0;2]
//      [5;0;7;1;0;0;7]
//      [0;0;0;7;4;3;6]]
// let s2 =
//     [[7;2;5;6;0;0;0]
//      [2;0;0;5;7;6;0]
//      [5;7;4;0;0;4;0]
//      [0;0;6;0;4;3;7]
//      [6;4;0;0;3;0;7]
//      [0;7;5;3;0;0;5]
//      [0;0;0;6;6;7;1]]
// let s3 =
//     [[7;2;0;0;4;7;0]
//      [0;6;7;5;2;0;0]
//      [4;5;0;5;0;0;6]
//      [0;0;0;7;7;1;5]
//      [6;7;3;0;0;0;4]
//      [3;0;6;0;0;6;5]
//      [0;0;4;3;7;6;0]]
// let s4 =
//     [[0;0;0;1;5;7;7]
//      [0;2;5;7;0;0;6]
//      [3;7;0;7;0;0;3]
//      [4;0;0;0;5;7;4]
//      [0;5;6;5;4;0;0]
//      [6;0;6;0;6;2;0]
//      [7;6;3;0;0;4;0]]
// let resultingGrid =
//     Array2D.init 7 7 (fun row col ->
//         s1.[row].[col]
//         + s2.[row].[col]
//         + s3.[row].[col]
//         + s4.[row].[col])
// [0..6]
// |> List.sumBy (fun row ->
//     Array.sum resultingGrid.[1,*])

