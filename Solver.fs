namespace Puzzle

module Solver =

    type private VectorType =
        | Row
        | Col

    let private potentialValuesPerCell (board: Board.Board) =
        seq {
            for r in 0 .. 6 do
                for c in 0 .. 6 -> r, c
        }
        |> Seq.filter (Board.isUnknown board)
        |> Seq.map
            (fun key ->
                key,
                Board.Empty
                :: ([ 1 .. 7 ] |> List.map (Board.Value)))
        |> Map.ofSeq

    let private emptyOrOnly cell cells =
        cells
        |> List.forall (fun c -> c = Board.Empty || c = cell)

    let private removeAllBut cell cells =
        cells
        |> List.filter (fun c -> c = Board.Empty || c = cell)

    let private removeNumbersForCell (toRemove: Set<_>) cells =
        cells |> List.filter (toRemove.Contains >> not)

    let private applyConstraints (constraints: Board.Constraint list)
                                 (board: Board.Board)
                                 (pvalues: Map<(int * int), Board.Cell list>)
                                 =
        let rec loop (acc: Map<_, _>) =
            let mutable changes = 0
            let mutable pvalues' = acc

            for c in constraints do
                let row, col, value, ok =
                    match c with
                    | Board.Top (index, value) ->
                        let rowIdx =
                            Board.firstNonEmptyIndexOfColumn index board

                        rowIdx, index, Board.Value value, rowIdx <= 4
                    | Board.Bottom (index, value) ->
                        let rowIdx =
                            Board.lastNonEmptyIndexOfColumn index board

                        rowIdx, index, Board.Value value, rowIdx >= 4
                    | Board.Left (index, value) ->
                        let colIdx =
                            Board.firstNonEmptyIndexOfRow index board

                        index, colIdx, Board.Value value, colIdx <= 4
                    | Board.Right (index, value) ->
                        let colIdx = Board.lastNonEmptyIndexOfRow index board
                        index, colIdx, Board.Value value, colIdx >= 4

                if ok
                   && (not (emptyOrOnly value pvalues'.[(row, col)])) then
                    changes <- changes + 1
                    let newCells = removeAllBut value pvalues'.[(row, col)]

                    if newCells.Length = 1 then
                        Board.setEmpty row col board
                        pvalues' <- pvalues'.Remove((row, col))
                    else
                        pvalues' <- pvalues'.Add((row, col), newCells)

            if changes = 0 then pvalues' else loop pvalues'

        loop pvalues

    let private first row constraints =
        constraints
        |> Seq.tryPick
            (function
            | Board.Left (i, v) when i = row -> Some v
            | _ -> None)

    let private last row constraints =
        constraints
        |> Seq.tryPick
            (function
            | Board.Right (i, v) when i = row -> Some v
            | _ -> None)

    let private top col constraints =
        constraints
        |> Seq.tryPick
            (function
            | Board.Top (i, v) when i = col -> Some v
            | _ -> None)

    let private bottom col constraints =
        constraints
        |> Seq.tryPick
            (function
            | Board.Bottom (i, v) when i = col -> Some v
            | _ -> None)

    // This function is creating all possible combinations that is possible for
    // the given setup. On an empty board this function returns in 7s for an empty
    // board and returns 5215 combinations.
    let private allPotentialVectors t idx constraints (pv: Map<_, _>) (board: Board.Board) =
        let firstValue, lastValue =
            match t with
            | Row -> first idx constraints, last idx constraints
            | Col -> top idx constraints, bottom idx constraints

        let rec firstValueIs list value =
            match list with
            | head :: tail when head = Board.Empty -> firstValueIs tail value
            | head :: tail when head = value -> true
            | head :: tail -> false
            | [] -> true

        let startCheck l =
            match firstValue with
            | Some v -> firstValueIs l (Board.Value v)
            | None -> true

        let endCheck l =
            match lastValue with
            | Some v -> firstValueIs (List.rev l) (Board.Value v)
            | None -> true

        let countCheck l =
            4 = List.length (
                List.choose
                    (function
                    | Board.Value v -> Some v
                    | _ -> None)
                    l
            )

        let sumCheck l =
            20 = List.sum (
                List.choose
                    (function
                    | Board.Value v -> Some v
                    | _ -> None)
                    l
            )

        let rec loop (acc: _ list) i =
            let row, col =
                match t with
                | Row -> (idx, i)
                | Col -> (i, idx)

            if i = 7 then
                acc
            else if pv.ContainsKey(row, col) then
                let acc' =
                    pv.[row, col]
                    |> List.collect (fun cell -> acc |> List.map (fun t -> cell :: t))

                loop acc' (i + 1)
            else
                let acc' =
                    acc |> List.map (fun t -> board.[row, col] :: t)

                loop acc' (i + 1)

        loop [ [] ] 0
        |> List.map (List.rev)
        |> List.filter (startCheck)
        |> List.filter (endCheck)
        |> List.filter (countCheck)
        |> List.filter (sumCheck)


    let allPotentialValues potentialVectors =
        potentialVectors
        |> List.map
            (fun l ->
                l
                |> List.filter
                    (function
                    | Board.Value _ -> true
                    | _ -> false)
                |> List.sortBy
                    (function
                    | Board.Value v -> v
                    | _ -> 0))
        |> Set.ofList
        |> Set.toList

    let removeNumbers (pv: Map<_, _>) board =
        let counter = Board.valueCounts board

        let numbersToRemove =
            counter
            |> Seq.indexed
            |> Seq.filter (fun (idx, counts) -> idx = counts - 1)
            |> Seq.map (snd >> Board.Value)
            |> Set.ofSeq

        Map.fold (fun (acc: Map<_, _>) k v -> acc.Add(k, removeNumbersForCell numbersToRemove pv.[k])) Map.empty pv

    let canSetInCol col cell board =
        match cell with
        | Board.Value v ->
            match Board.valuesInCol col board with
            | 0 -> true
            | 1 -> v + (Board.colSum col board) >= 6
            | 2 -> v + (Board.colSum col board) >= 13
            | 3 -> v = 20 - (Board.colSum col board)
            | _ -> false
        | Board.Empty -> Board.emptiesInCol col board <= 2
        | Board.Unknown -> false

    let private handleVector t index (vector: _ list) (pv: Map<_, _>) (board: Board.Board) =
        let counter = Board.valueCounts board

        vector
        |> List.indexed
        |> List.iter
            (function
            | i, Board.Value v ->
                if Board.isUnknown board (index, i) then counter.[v - 1] <- counter.[v - 1] + 1 else ()
            | _ -> ())

        let valid =
            counter
            |> Array.indexed
            |> Array.forall (fun (i, v) -> i + 1 >= v)

        if valid then
            match t with
            | Row ->
                if vector
                   |> List.indexed
                   |> List.forall
                       (fun (i, cell) ->
                           let canSet =
                               if Board.isUnknown board (index, i) then canSetInCol i cell board else true

                           let isOk =
                               if pv.ContainsKey(index, i) then List.contains cell pv.[index, i] else true

                           canSet && isOk) then
                    // updates the board with values from vector and keeps track of the indexes
                    // changed for backtracking.
                    let indexesChanged =
                        [ 0 .. 6 ]
                        |> List.choose
                            (fun col ->
                                if board.[index, col] = Board.Unknown then
                                    board.[index, col] <- vector.[col]
                                    Some col
                                else
                                    None)

                    Some(removeNumbers pv board, indexesChanged)
                else
                    None
            | Col ->
                let indexesChanged =
                    [ 0 .. 6 ]
                    |> List.choose
                        (fun row ->
                            if board.[row, index] = Board.Unknown then
                                board.[row, index] <- vector.[row]
                                Some row
                            else
                                None)

                Some(removeNumbers pv board, indexesChanged)
        else
            None

    let private undoVector t index (vector: _ list) changedIndexes board =
        match t with
        | Row ->
            changedIndexes
            |> List.iter (fun col -> Board.resetAt index col board)
        | Col ->
            changedIndexes
            |> List.iter (fun row -> Board.resetAt row index board)


    let searchSolutionForBoard (constraints: Board.Constraint list) (board: Board.Board) =
        let potentialValues =
            potentialValuesPerCell board
            |> applyConstraints constraints board

        let potentialRows =
            [ 0 .. 6 ]
            |> List.map (fun idx -> idx, allPotentialVectors Row idx constraints potentialValues board)
            |> List.sortBy (fun (_, l) -> l.Length)

        let rec rowSolver pvs (potentialRows: (int * _ list) list) =
            match potentialRows with
            | (i, pRows) :: tail ->
                let result =
                    List.tryPick
                        (fun row ->
                            match handleVector Row i row pvs board with
                            | Some (pvs', mem) ->
                                if rowSolver pvs' tail then
                                    Some true
                                else
                                    undoVector Row i row mem board
                                    None
                            | None -> None)
                        pRows

                match result with
                | Some true -> true
                | _ -> false
            | [] -> Verifier.verifySolution false constraints board

        rowSolver potentialValues potentialRows
