namespace Puzzle

module Verifier =

    open System.Collections.Generic

    let private printIf verbose s = if verbose then printfn "%s" s

    let verifySolution verbose (ctx: Board.Constraint list) (board: Board.Board) =
        let mutable result = true

        for row in 0 .. 6 do
            let vs = Board.rowValues row board

            if vs.Length <> 4 then
                result <- false
                printIf verbose (sprintf "Row %d has not 4 values" row)

            if Array.sum vs <> 20 then
                result <- false
                printIf verbose (sprintf "Row %d's sum is not 20. It is %d" row (Array.sum vs))

        for col in 0 .. 6 do
            let vs = Board.colValues col board

            if vs.Length <> 4 then
                result <- false
                printIf verbose (sprintf "Col %d has not 4 values" col)

            if Array.sum vs <> 20 then
                result <- false
                printIf verbose (sprintf "Col %d's sum is not 20. It is %d" col (Array.sum vs))

        for fConstraint in ctx do
            match fConstraint with
            | Board.Top (idx, valueWant) ->
                let valueGot = Board.firstColValue idx board

                if valueWant <> valueGot then
                    result <- false
                    printIf verbose (sprintf "First value in column %d should be %d but was %d" idx valueWant valueGot)
            | Board.Bottom (idx, valueWant) ->
                let valueGot = Board.lastColValue idx board

                if valueWant <> valueGot then
                    result <- false
                    printIf verbose (sprintf "last value in column %d should be %d but was %d" idx valueWant valueGot)
            | Board.Left (idx, valueWant) ->
                let valueGot = Board.firstRowValue idx board

                if valueWant <> valueGot then
                    result <- false
                    printIf verbose (sprintf "First value in row %d should be %d but was %d" idx valueWant valueGot)
            | Board.Right (idx, valueWant) ->
                let valueGot = Board.lastRowValue idx board

                if valueWant <> valueGot then
                    result <- false
                    printIf verbose (sprintf "Last value in row %d should be %d but was %d" idx valueWant valueGot)

        let startingCol = Board.firstNonEmptyIndexOfRow 0 board
        let visited = HashSet<int * int>()
        let deltas = [ (-1, 0); (0, -1); (1, 0); (0, 1) ]

        let rec dfs stack =
            match stack with
            | (row, col) as head :: tail ->
                if visited.Contains head then
                    dfs tail
                else
                    visited.Add head |> ignore

                    dfs (
                        (deltas
                         |> List.map (fun (dr, dc) -> row + dr, col + dc)
                         |> List.filter
                             (fun (r, c) ->
                                 if 0 <= r && r <= 6 && 0 <= c && c <= 6 then
                                     match board.[r, c] with
                                     | Board.Value _ -> true
                                     | _ -> false
                                 else
                                     false)
                         |> List.filter (visited.Contains >> not))
                        @ stack
                    )
            | [] -> ()

        dfs [ (0, startingCol) ]

        if visited.Count <> 28 then
            result <- false
            printIf verbose "Not all values are connected"

        let oneEmptyInEvery2x2Grid =
            seq {
                for r in 0 .. 5 do
                    for c in 0 .. 5 -> r, c
            }
            |> Seq.forall
                (fun (r, c) ->
                    [ (0, 0); (1, 0); (0, 1); (1, 1) ]
                    |> List.exists (fun (dr, dc) -> board.[r + dr, c + dc] = Board.Empty))

        if not oneEmptyInEvery2x2Grid then
            result <- false
            printIf verbose "A 2x2 grid is completely filled"

        result
