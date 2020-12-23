namespace Puzzle

module Board =

    type Index = int

    type Constraint =
        | Top of Index * int
        | Bottom of Index * int
        | Left of Index * int
        | Right of Index * int

    type Cell =
        | Empty
        | Unknown
        | Value of int

    let cellToString cell =
        match cell with
        | Unknown -> "  "
        | Empty -> "# "
        | Value v -> sprintf "%d " v

    type Board = Cell [,]

    let makeBoard (): Board = Array2D.init 7 7 (fun _ _ -> Unknown)

    let print (board: Board) =
        printfn "- - - - - - -"

        [ 0 .. 6 ]
        |> List.iter
            (fun row ->
                board.[row, *]
                |> Seq.iter (cellToString >> (printf "%s"))

                printfn "")

        printfn "- - - - - - -"

    let rowSum rowIdx (board: Board) =
        board.[rowIdx, *]
        |> Array.choose
            (fun v ->
                match v with
                | Value v -> Some v
                | _ -> None)
        |> Array.sum

    let colSum colIdx (board: Board) =
        board.[*, colIdx]
        |> Array.choose
            (fun v ->
                match v with
                | Value v -> Some v
                | _ -> None)
        |> Array.sum

    let firstRowValue rowIdx (board: Board) =
        board.[rowIdx, *]
        |> Seq.choose
            (function
            | Value v -> Some v
            | _ -> None)
        |> Seq.head

    let firstNonEmptyIndexOfRow rowIdx (board: Board) =
        board.[rowIdx, *]
        |> Seq.indexed
        |> Seq.filter
            (function
            | (i, Empty) -> false
            | _ -> true)
        |> Seq.map fst
        |> Seq.head

    let lastRowValue rowIdx (board: Board) =
        board.[rowIdx, *]
        |> Seq.choose
            (function
            | Value v -> Some v
            | _ -> None)
        |> Seq.last

    let lastNonEmptyIndexOfRow rowIdx (board: Board) =
        board.[rowIdx, *]
        |> Seq.indexed
        |> Seq.filter
            (function
            | (i, Empty) -> false
            | _ -> true)
        |> Seq.map fst
        |> Seq.last

    let firstColValue colIdx (board: Board) =
        board.[*, colIdx]
        |> Seq.choose
            (function
            | Value v -> Some v
            | _ -> None)
        |> Seq.head

    let firstNonEmptyIndexOfColumn colIdx (board: Board) =
        board.[*, colIdx]
        |> Seq.indexed
        |> Seq.filter
            (function
            | (i, Empty) -> false
            | _ -> true)
        |> Seq.map fst
        |> Seq.head

    let lastColValue colIdx (board: Board) =
        board.[*, colIdx]
        |> Seq.choose
            (function
            | Value v -> Some v
            | _ -> None)
        |> Seq.last

    let lastNonEmptyIndexOfColumn colIdx (board: Board) =
        board.[*, colIdx]
        |> Seq.indexed
        |> Seq.filter
            (function
            | (i, Empty) -> false
            | _ -> true)
        |> Seq.map fst
        |> Seq.last

    let emptiesInRow rowIdx (board: Board) =
        board.[rowIdx, *]
        |> Seq.filter
            (function
            | Empty -> true
            | _ -> false)
        |> Seq.length

    let valuesInRow rowIdx (board: Board) =
        board.[rowIdx, *]
        |> Seq.filter
            (function
            | Value _ -> true
            | _ -> false)
        |> Seq.length

    let emptiesInCol colIdx (board: Board) =
        board.[*, colIdx]
        |> Seq.filter
            (function
            | Empty -> true
            | _ -> false)
        |> Seq.length

    let valuesInCol colIdx (board: Board) =
        board.[*, colIdx]
        |> Seq.filter
            (function
            | Value _ -> true
            | _ -> false)
        |> Seq.length

    let resetAt row col (board: Board) = board.[row, col] <- Unknown

    let setEmpty row col (board: Board) = board.[row, col] <- Empty

    let updateValue row col value (board: Board) = board.[row, col] <- Value value

    let valueAt row col (board: Board) =
        match board.[row, col] with
        | Value v -> v
        | Empty -> 0
        | Unknown -> 0

    let rowValues rowIdx (board: Board) =
        board.[rowIdx, *]
        |> Array.choose
            (fun v ->
                match v with
                | Value v -> Some v
                | _ -> None)

    let colValues colIdx (board: Board) =
        board.[*, colIdx]
        |> Array.choose
            (fun v ->
                match v with
                | Value v -> Some v
                | _ -> None)

    let isUnknown (board: Board) (r, c) = board.[r, c] = Unknown

    let valueCounts (board: Board) =
        let arr = Array.zeroCreate 7

        [ 0 .. 6 ]
        |> Seq.iter
            (fun row ->
                board.[row, *]
                |> Seq.iter
                    (function
                    | Value v -> arr.[v - 1] <- arr.[v - 1] + 1
                    | _ -> ()))

        arr
