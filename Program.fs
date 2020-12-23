namespace Puzzle

module Main =

    open System.IO

    [<EntryPoint>]
    let main argv =
        if argv.Length <> 2 then
            printfn
                "expect exactly 2 arguments, a filename with a puzzle input and an output file. The output file shouldn't exist!"

            1
        else
            let filename = argv.[0]
            let resultFilename = argv.[1]

            if File.Exists resultFilename
               || not (File.Exists filename) then
                if File.Exists resultFilename
                then printfn "The second argument is the output file and it will be created. The file shouldn't exist!"

                if not (File.Exists filename)
                then printfn "The first argument is the input file. The file should exist!"

                1
            else
                printfn "Read puzzle from %s" filename
                let board, constraints = IO.readFile filename
                printfn "The starting board is:"
                Board.print board
                // The board will be mutated when searching for a solution.
                // When a solution is found the board will represent that
                // solution.
                let solutionFound =
                    Solver.searchSolutionForBoard constraints board

                if solutionFound then
                    printfn "Found a solution!!"
                    Board.print board
                    IO.toFile resultFilename board
                else
                    printfn "No solution found"

                0
