namespace Puzzle

module IO = 
    open System.IO

    let readFile filename =
        if not (File.Exists filename)
        then failwithf "file '%s' not found. Does it exist?" filename

        let board = Board.makeBoard()
        let mutable constraints = []
        let lines = File.ReadAllLines filename

        // Validate that the input is like expected:
        let arr =
            lines
            |> Seq.map ((fun line -> line.Split(",") |> Seq.map int))
            |> array2D
        if arr.GetLength(0) <> 9 || arr.GetLength(1) <> 9
        then failwith "expect the input grid in a 9x9 grid of integers seperated by ','"
        if not (lines |> Seq.collect ((fun line -> line.Split(",") |> Seq.map int)) |> Seq.forall (fun x -> 0 <= x && x <= 7))
        then failwith "expect all integers in the 9x9 grid to be in [0, 7]"

        // parse the input:
        lines
        |> Seq.map (fun l -> l.Split(",") |> Seq.map int |> Seq.indexed)
        |> Seq.indexed
        |> Seq.iter (fun (rowIdx, row) ->
            row
            |> Seq.iter (fun (colIdx, value) ->
                if value > 0 then
                    match rowIdx, colIdx with
                    | 0, 0
                    | 0, 8
                    | 8, 0
                    | 8, 8 -> ()
                    | 0, _ ->
                        if 0 < value && value <= 7
                        then constraints <- (Board.Top (colIdx - 1, value))::constraints
                    | 8, _ -> 
                        if 0 < value && value <= 7
                        then constraints <- (Board.Bottom (colIdx - 1, value))::constraints
                    | _, 0 -> 
                        if 0 < value && value <= 7
                        then constraints <- (Board.Left (rowIdx - 1, value))::constraints
                    | _, 8 -> 
                        if 0 < value && value <= 7
                        then constraints <- (Board.Right (rowIdx - 1, value))::constraints
                    | _ ->
                        if 0 < value && value <= 7
                        then Board.updateValue (rowIdx - 1) (colIdx - 1) value board))
        
        board, constraints

    let toFile filename board = 
        let lines =
            [0..6]
            |> Seq.map (fun row ->
                [0..6]
                |> Seq.map (fun col -> Board.valueAt row col board)
                |> System.String.Concat)

        File.WriteAllLines(filename, lines)