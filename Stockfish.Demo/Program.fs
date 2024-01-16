#if INTERACTIVE
#load "../Stockfish.Lib/Library.fs"
#else
module Stockfish.Demo
#endif

open Stockfish.Lib

open System

#if !INTERACTIVE
[<EntryPoint>]
#endif
let main args =
    ignore args

    use sf = new Stockfish("win-stockfish16.exe")

    sf.GetParameters()
    |> Seq.map (fun x -> x.Deconstruct())
    |> Map
    |> Map.iter (fun k v -> printfn "%s=%A" k v)

    let rec loop i =
        if i = -1 then
            ()
        else
            let turn = sf.GetFenParts().Value.SideToMove
            let isWhite = turn = "w"
            let board = sf.GetBoardVisual(isWhite)
            printfn "%s" board

            printfn "Make a move"

            let input = Console.ReadLine()

            if sf.IsMoveValid(input) then
                sf.MakeMovesFromCurrentPosition(input.Trim().Split(" "))
            else
                eprintfn "Invalid move...\nTry again\n\n"

            loop (i + 1)

    loop 1

    0

#if INTERACTIVE
main fsi.CommandLineArgs
#endif
