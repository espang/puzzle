namespace Puzzle


module Utils =

    let private sums =
        let mutable m = Map.empty

        [ 1 .. 7 ]
        |> Seq.iter (fun s -> m <- m.Add((s, 1), [ [ s ] ]))

        [ 6 .. 14 ]
        |> Seq.iter
            (fun s ->
                m <-
                    m.Add(
                        (s, 2),
                        [ 1 .. s / 2 ]
                        |> List.choose (fun s1 -> if s - s1 <= 7 then Some [ s1; s - s1 ] else None)
                    ))

        [ 13 .. 19 ]
        |> Seq.iter
            (fun s ->
                m <-
                    m.Add(
                        (s, 3),
                        [ for s1 in 1 .. s / 3 do
                            for s2 in s1 .. 7 -> s1, s2 ]
                        |> List.choose
                            (fun (s1, s2) ->
                                let s3 = s - s1 - s2
                                if s2 <= s3 && s3 <= 7 then Some [ s1; s2; s3 ] else None)
                    ))

        m <-
            m.Add(
                (20, 4),
                [ for s1 in 1 .. 5 do
                    for s2 in s1 .. 7 do
                        for s3 in s2 .. 7 -> s1, s2, s3 ]
                |> List.choose
                    (fun (s1, s2, s3) ->
                        let s4 = 20 - s1 - s2 - s3
                        if s3 <= s4 && s4 <= 7 then Some [ s1; s2; s3; s4 ] else None)
            )

        m

    let sumsLookup sum n = sums.TryFind(sum, n)


    // Appends all lists from lists to the list:
    // As an example `combine [[a] [b] [c]] [7]`
    // would return [ [7 a] [7 b] [7 c] ]
    let combine lists list =
        let rec loop acc lists =
            match lists with
            | head :: tail -> loop ((list @ head) :: acc) tail
            | [] -> acc

        loop [] lists
