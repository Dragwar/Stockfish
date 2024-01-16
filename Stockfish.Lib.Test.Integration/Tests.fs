module Stockfish.Lib.Test.Integration.Tests

open Xunit
open FsUnit.Xunit

open Stockfish.Lib

open System
open System.Threading
open System.Threading.Tasks
open System.Collections.Generic

module TestUtil =
    let dictEq (d1: IReadOnlyDictionary<string, obj>) (d2: IReadOnlyDictionary<string, obj>) : bool =
        seq {
            for d1p in d1 do
                for d2p in d2 do
                    match (d1p.Value, d2.TryGetValue(d1p.Key)), (d2p.Value, d1.TryGetValue(d2p.Key)) with
                    | (d1Expected, (true, d2Value)), (d2Expected, (true, d1Value)) ->
                        d1Expected = d2Value && d2Expected = d1Value
                    | _ -> false
        }
        |> Seq.forall id

let inline increment<'a when 'a: (static member (+): 'a * 'a -> 'a)> (x: byref<'a>) (v: 'a) = x <- x + v
let inline (+=) (x: byref<_>) (v: _) = increment &x v

type StockfishTests() =
    let EXE_PATH =
        match (OperatingSystem.IsWindows(), OperatingSystem.IsMacOS(), OperatingSystem.IsLinux()) with
        | (true, _, _) -> "win-stockfish16.exe"
        | (_, true, _) -> "mac-stockfish16"
        | (_, _, true) -> "lin-stockfish16"
        | _ -> "stockfish"
        //|> (+) "../stockfish-cli/"

    let stockfish = lazy new Stockfish(EXE_PATH)

    [<Fact>]
    let ``GetBestMove for first move`` () =
        let bestMove = stockfish.Value.GetBestMove()

        [
            "e2e3"
            "e2e4"
            "g1f3"
            "b1c3"
            "d2d4"
        ]
        |> List.map Some
        |> should contain bestMove

    [<Fact>]
    let ``GetBestMoveTime for first move`` () =
        let bestMove = stockfish.Value.GetBestMoveTime(1000)

        [
            "e2e3"
            "e2e4"
            "g1f3"
            "b1c3"
            "d2d4"
        ]
        |> List.map Some
        |> should contain bestMove

    [<Fact>]
    let ``GetBestMove with remaining time for first move`` () =
        let bestMove = stockfish.Value.GetBestMove(wtime = 1000)

        [
            "a2a3"
            "d2d4"
            "e2e4"
            "g1f3"
            "c2c4"
        ]
        |> List.map Some
        |> should contain bestMove

        let bestMove = stockfish.Value.GetBestMove(btime = 1000)

        [ "g1f3"; "d2d4"; "e2e4"; "c2c4" ]
        |> List.map Some
        |> should contain bestMove

        let bestMove =
            stockfish.Value.GetBestMove(wtime = 1000, btime = 1000)

        [
            "g2g3"
            "g1f3"
            "e2e4"
            "d2d4"
            "c2c4"
            "e2e3"
        ]
        |> List.map Some
        |> should contain bestMove

        let bestMove =
            stockfish.Value.GetBestMove(wtime = 5 * 60 * 1000, btime = 1000)

        [
            "e2e3"
            "e2e4"
            "g1f3"
            "b1c3"
            "d2d4"
        ]
        |> List.map Some
        |> should contain bestMove

    [<Fact>]
    let ``SetPosition resets "Info"`` () =
        stockfish.Value.SetPosition([ "e2e4"; "e7e6" ])
        stockfish.Value.GetBestMove() |> ignore
        stockfish.Value.Info |> should not' (be EmptyString)
        stockfish.Value.SetPosition([ "e2e4"; "e7e6" ])
        stockfish.Value.Info |> should be EmptyString

    [<Fact>]
    let ``GetBestMove from non-starting position`` () =
        stockfish.Value.SetPosition([ "e2e4"; "e7e6" ])
        let bestMove = stockfish.Value.GetBestMove()

        [ "d2d4"; "g1f3" ]
        |> List.map Some
        |> should contain bestMove

    [<Fact>]
    let ``GetBestMoveTime from non-starting position`` () =
        stockfish.Value.SetPosition([ "e2e4"; "e7e6" ])
        let bestMove = stockfish.Value.GetBestMoveTime(1000)

        [ "d2d4"; "g1f3" ]
        |> List.map Some
        |> should contain bestMove

    [<Fact>]
    let ``GetBestMove with remaining time from non-starting position`` () =
        stockfish.Value.SetPosition([ "e2e4"; "e7e6" ])
        let bestMove = stockfish.Value.GetBestMove(wtime = 1000)

        [ "d2d4"; "a2a3"; "d1e2"; "b1c3" ]
        |> List.map Some
        |> should contain bestMove

        let bestMove = stockfish.Value.GetBestMove(btime = 1000)

        [ "d2d4"; "b1c3" ]
        |> List.map Some
        |> should contain bestMove

        let bestMove =
            stockfish.Value.GetBestMove(wtime = 1000, btime = 1000)

        [ "d2d4"; "b1c3"; "g1f3" ]
        |> List.map Some
        |> should contain bestMove

        let bestMove =
            stockfish.Value.GetBestMove(wtime = 5 * 60 * 1000, btime = 1000)

        [
            "e2e3"
            "e2e4"
            "g1f3"
            "b1c3"
            "d2d4"
        ]
        |> List.map Some
        |> should contain bestMove

    [<Fact>]
    let ``GetBestMove while in checkmate`` () =
        stockfish.Value.SetPosition([ "f2f3"; "e7e5"; "g2g4"; "d8h4" ])
        stockfish.Value.GetBestMove() |> should equal None

    [<Fact>]
    let ``GetBestMoveTime while in checkmate`` () =
        stockfish.Value.SetPosition([ "f2f3"; "e7e5"; "g2g4"; "d8h4" ])
        stockfish.Value.GetBestMoveTime(1000) |> should equal None

    [<Fact>]
    let ``GetBestMove with remaining time while in checkmate`` () =
        stockfish.Value.SetPosition([ "f2f3"; "e7e5"; "g2g4"; "d8h4" ])

        stockfish.Value.GetBestMove(wtime = 1000)
        |> should equal None

        stockfish.Value.GetBestMove(btime = 1000)
        |> should equal None

        stockfish.Value.GetBestMove(wtime = 1000, btime = 1000)
        |> should equal None

        stockfish.Value.GetBestMove(wtime = 5 * 60 * 1000, btime = 1000)
        |> should equal None

    [<Fact>]
    let ``SetFenPosition plain move`` () =
        stockfish.Value.SetFenPosition("7r/1pr1kppb/2n1p2p/2NpP2P/5PP1/1P6/P6K/R1R2B2 w - - 1 27")
        stockfish.Value.IsMoveCorrect("f4f5") |> should be True
        stockfish.Value.IsMoveCorrect("a1c1") |> should be False

    [<Fact>]
    let ``SetFenPosition castling`` () =
        stockfish.Value.IsMoveCorrect("e1g1") |> should be False
        stockfish.Value.SetFenPosition("rnbqkbnr/ppp3pp/3ppp2/8/4P3/5N2/PPPPBPPP/RNBQK2R w KQkq - 0 4")
        stockfish.Value.IsMoveCorrect("e1g1") |> should be True

    [<Fact>]
    let ``SetFenPosition in checkmate`` () =
        stockfish.Value.SetFenPosition("8/8/8/6pp/8/4k1PP/8/r3K3 w - - 12 53")
        stockfish.Value.GetBestMove() |> should equal None

        stockfish.Value.Info
        |> should equal "info depth 0 score mate 0"

    [<Fact>]
    let ``SetFenPosition should clear "Info"`` () =
        stockfish.Value.SetFenPosition("8/8/8/6pp/8/4k1PP/r7/4K3 b - - 11 52")
        stockfish.Value.GetBestMove() |> ignore
        stockfish.Value.SetFenPosition("8/8/8/6pp/8/4k1PP/8/r3K3 w - - 12 53")
        stockfish.Value.Info |> should be EmptyString

        stockfish.Value.SetFenPosition("8/8/8/6pp/8/4k1PP/r7/4K3 b - - 11 52")
        stockfish.Value.GetBestMove() |> ignore
        stockfish.Value.SetFenPosition("8/8/8/6pp/8/4k1PP/8/r3K3 w - - 12 53", false)
        stockfish.Value.Info |> should be EmptyString

    [<Fact>]
    let ``SetFenPosition starts new game`` () =
        stockfish.Value.SetFenPosition("7r/1pr1kppb/2n1p2p/2NpP2P/5PP1/1P6/P6K/R1R2B2 w - - 1 27")
        stockfish.Value.GetBestMove() |> ignore
        stockfish.Value.Info |> should not' (be EmptyString)
        stockfish.Value.SetFenPosition("3kn3/p5rp/1p3p2/3B4/3P1P2/2P5/1P3K2/8 w - - 0 53")
        stockfish.Value.Info |> should be EmptyString

    [<Fact>]
    let ``SetFenPosition with ucinewgame flag`` () =
        stockfish.Value.SetDepth(16)
        stockfish.Value.SetFenPosition("rnbqk2r/pppp1ppp/3bpn2/8/3PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 0 1", true)

        stockfish.Value.GetBestMove() |> should equal (Some "e4e5")

        stockfish.Value.SetFenPosition("rnbqk2r/pppp1ppp/3bpn2/4P3/3P4/2N5/PPP2PPP/R1BQKBNR b KQkq - 0 1", false)

        stockfish.Value.GetBestMove()
        |> fun v -> [ Some "d6e7"; Some "d6b4" ] |> Seq.contains v
        |> should be True
        //|> should equal (Some "d6e7")

        stockfish.Value.SetFenPosition("rnbqk2r/pppp1ppp/3bpn2/8/3PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 0 1", false)

        stockfish.Value.GetBestMove() |> should equal (Some "e4e5")

    [<Fact>]
    let ``GetFenParts uses current position`` () =
        stockfish.Value.SetFenPosition("rnbqk2r/pppp1ppp/3bpn2/8/3PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 0 1", true)
        let parts = stockfish.Value.GetFenParts()
        parts.IsSome |> should be True

        parts.Value.PiecePlacement
        |> should equal "rnbqk2r/pppp1ppp/3bpn2/8/3PP3/2N5/PPP2PPP/R1BQKBNR"

        parts.Value.RankItem
        |> Seq.toList
        |> (=) [
            "rnbqk2r"
            "pppp1ppp"
            "3bpn2"
            "8"
            "3PP3"
            "2N5"
            "PPP2PPP"
            "R1BQKBNR"
        ]
        |> should be True

        parts.Value.SideToMove |> should equal "w"
        parts.Value.Castling |> should equal "KQkq"
        parts.Value.EnPassant |> should equal "-"
        parts.Value.HalfMoveClock |> should equal "0"
        parts.Value.FullMoveNumber |> should equal "1"

    [<Fact>]
    let ``GetFenParts uses passed in fen`` () =
        let parts =
            stockfish.Value.GetFenParts("rnbqk2r/pppp1ppp/3bpn2/8/3PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 0 1")

        parts.IsSome |> should be True

        parts.Value.PiecePlacement
        |> should equal "rnbqk2r/pppp1ppp/3bpn2/8/3PP3/2N5/PPP2PPP/R1BQKBNR"

        parts.Value.RankItem
        |> Seq.toList
        |> (=) [
            "rnbqk2r"
            "pppp1ppp"
            "3bpn2"
            "8"
            "3PP3"
            "2N5"
            "PPP2PPP"
            "R1BQKBNR"
        ]
        |> should be True

        parts.Value.SideToMove |> should equal "w"
        parts.Value.Castling |> should equal "KQkq"
        parts.Value.EnPassant |> should equal "-"
        parts.Value.HalfMoveClock |> should equal "0"
        parts.Value.FullMoveNumber |> should equal "1"

    [<Fact>]
    let ``IsMoveValid for first move`` () =
        let expecetedInfo = stockfish.Value.Info
        let expectedPosition = stockfish.Value.GetFenPosition()
        stockfish.Value.IsMoveValid("e1e7") |> should be False
        stockfish.Value.IsMoveValid("a9a1") |> should be False
        stockfish.Value.IsMoveValid("d2d4") |> should be True
        stockfish.Value.IsMoveValid("a2a3") |> should be True

        stockfish.Value.Info |> should equal expecetedInfo

        stockfish.Value.GetFenPosition()
        |> should equal expectedPosition

    [<Fact>]
    let ``IsMoveValid from non-starting position`` () =
        stockfish.Value.SetPosition([ "e2e4"; "e7e6" ])
        let expecetedInfo = stockfish.Value.Info
        let expectedPosition = stockfish.Value.GetFenPosition()
        stockfish.Value.IsMoveValid("e1e7") |> should be False
        stockfish.Value.IsMoveValid("a9a1") |> should be False
        stockfish.Value.IsMoveValid("d2d4") |> should be True
        stockfish.Value.IsMoveValid("a2a3") |> should be True

        stockfish.Value.Info |> should equal expecetedInfo

        stockfish.Value.GetFenPosition()
        |> should equal expectedPosition

    [<Fact>]
    let ``IsMoveCorrect for first move`` () =
        stockfish.Value.IsMoveCorrect("e2e1") |> should be False
        stockfish.Value.IsMoveCorrect("a2a3") |> should be True

    [<Fact>]
    let ``IsMoveCorrect from non-starting position`` () =
        stockfish.Value.SetPosition([ "e2e4"; "e7e6" ])
        stockfish.Value.IsMoveCorrect("e2e1") |> should be False
        stockfish.Value.IsMoveCorrect("a2a3") |> should be True

    [<MemberData(nameof StockfishTests.``GetBestMove should update "Info" - parameters``)>]
    [<Theory>]
    let ``GetBestMove should update "Info"`` (value: string array) =
        stockfish.Value.SetFenPosition("r6k/6b1/2b1Q3/p6p/1p5q/3P2PP/5r1K/8 w - - 1 31")
        stockfish.Value.GetBestMove() |> ignore

        for v in value do
            stockfish.Value.Info.Contains v |> should be True

    [<Fact>]
    let ``SetSkillLevel`` () =
        stockfish.Value.SetFenPosition("rnbqkbnr/ppp2ppp/3pp3/8/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 0 1")

        stockfish.Value.GetParameters().["Skill Level"]
        |> should equal 20

        stockfish.Value.SetSkillLevel(1)

        [
            "b2b3"
            "d2d3"
            "d2d4"
            "b1c3"
            "d1e2"
            "g2g3"
            "c2c4"
            "f1e2"
            "c2c3"
            "h2h3"
            "f1b5"
        ]
        |> Seq.map Some
        |> should contain (stockfish.Value.GetBestMove())

        stockfish.Value.GetParameters().["Skill Level"]
        |> should equal 1

        stockfish.Value.GetParameters().["UCI_LimitStrength"]
        |> should equal "false"

        stockfish.Value.SetSkillLevel(20)

        [ "d2d4"; "c2c4" ]
        |> Seq.map Some
        |> should contain (stockfish.Value.GetBestMove())

        stockfish.Value.GetParameters().["Skill Level"]
        |> should equal 20

        stockfish.Value.GetParameters().["UCI_LimitStrength"]
        |> should equal "false"

    [<Fact>]
    let ``SetEloRating`` () =
        stockfish.Value.SetFenPosition("rnbqkbnr/ppp2ppp/3pp3/8/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 0 1")

        stockfish.Value.GetParameters().["UCI_Elo"]
        |> should equal 1350

        stockfish.Value.SetEloRating(2000)

        [
            "d2d4"
            "b1c3"
            "d1e2"
            "c2c4"
            "f1e2"
            "h2h3"
            "c2c3"
            "f1d3"
            "a2a3"
        ]
        |> Seq.map Some
        |> should contain (stockfish.Value.GetBestMove())

        stockfish.Value.GetParameters().["UCI_Elo"]
        |> should equal 2000

        stockfish.Value.GetParameters().["UCI_LimitStrength"]
        |> should equal "true"

        stockfish.Value.SetEloRating(1350)

        [
            "d1e2"
            "b1c3"
            "d2d3"
            "d2d4"
            "c2c4"
            "f1e2"
            "c2c3"
            "f1b5"
            "g2g3"
            "h2h3"
        ]
        |> Seq.map Some
        |> should contain (stockfish.Value.GetBestMove())

        stockfish.Value.GetParameters().["UCI_Elo"]
        |> should equal 1350

        stockfish.Value.GetParameters().["UCI_LimitStrength"]
        |> should equal "true"

        stockfish.Value.SetEloRating(2850)

        let majorVersion = stockfish.Value.StockfishMajorVersion

        let expectedBestMoves =
            [
                "d2d4"
                "b1c3"
                "c2c3"
                "c2c4"
                "f1b5"
                "f1e2"
            ]
            |> ResizeArray

        if
            majorVersion >= 12
            && stockfish.Value.IsDevelopmentBuildOfEngine() |> not
        then
            expectedBestMoves.Remove("f1e2") |> ignore

        expectedBestMoves
        |> Seq.map Some
        |> should contain (stockfish.Value.GetBestMove())

        stockfish.Value.GetParameters().["UCI_Elo"]
        |> should equal 2850

    [<Fact>]
    let ``Common Parameter Checks`` () =
        let toDict (v: (string * obj) seq) =
            Dictionary<string, obj>(v |> Seq.map KeyValuePair.Create)

        let oldParameters =
            toDict [
                "Debug Log File", ""
                "Contempt", 0
                "Min Split Depth", 0
                "Threads", 1
                "Ponder", "false"
                "Hash", 16
                "MultiPV", 1
                "Skill Level", 20
                "Move Overhead", 10
                "Minimum Thinking Time", 20
                "Slow Mover", 100
                "UCI_Chess960", "false"
                "UCI_LimitStrength", "false"
                "UCI_Elo", 1350
            ]

        let expectedParameters = oldParameters |> Dictionary
        stockfish.Value.SetSkillLevel(1)
        expectedParameters.["Skill Level"] <- 1

        stockfish.Value.GetParameters()
        |> TestUtil.dictEq expectedParameters
        |> should be True
        //stockfish.Value._DEFAULT_STOCKFISH_PARAMS
        //|> TestUtil.dictEq oldParameters
        //|> should be True

        stockfish.Value.SetSkillLevel(20)
        expectedParameters.["Skill Level"] <- 20

        stockfish.Value.GetParameters()
        |> TestUtil.dictEq oldParameters
        |> should be True
        //stockfish.Value._DEFAULT_STOCKFISH_PARAMS
        //|> TestUtil.dictEq oldParameters
        //|> should be True

        stockfish.Value.UpdateEngineParameters(readOnlyDict [ "Threads", 4 ])
        expectedParameters.["Threads"] <- 4

        stockfish.Value.GetParameters()
        |> TestUtil.dictEq expectedParameters
        |> should be True

        stockfish.Value.UpdateEngineParameters(readOnlyDict [ "Hash", 128 ])
        expectedParameters.["Hash"] <- 128

        stockfish.Value.GetParameters()
        |> TestUtil.dictEq expectedParameters
        |> should be True

        stockfish.Value.UpdateEngineParameters(readOnlyDict [ "Hash", 256; "Threads", 3 ])

        [ "Hash", 256; "Threads", 3 ]
        |> Seq.iter (expectedParameters.AddOrUpdate)

        stockfish.Value.GetParameters()
        |> TestUtil.dictEq expectedParameters
        |> should be True

    [<Fact>]
    let ``UCI_Chess960_position`` () =
        stockfish.Value
            .GetFenPosition()
            .Contains
            "KQkq"
        |> should be True

        let oldParameters = stockfish.Value.GetParameters()

        let expectedParameters =
            stockfish.Value.GetParameters() |> Dictionary

        expectedParameters.["UCI_Chess960"] <- "true"
        stockfish.Value.UpdateEngineParameters(readOnlyDict [ "UCI_Chess960", "true" ])

        stockfish.Value
            .GetFenPosition()
            .Contains
            "HAha"
        |> should be True

        stockfish.Value.GetParameters()
        |> TestUtil.dictEq expectedParameters
        |> should be True

        stockfish.Value.SetFenPosition("4rkr1/4p1p1/8/8/8/8/8/4nK1R w K - 0 100")

        stockfish.Value.GetBestMove() |> should equal (Some "f1h1")

        stockfish.Value.GetEvaluation()
        |> TestUtil.dictEq (readOnlyDict [ "type", "mate"; "value", 2 ])
        |> should be True

        stockfish.Value.WillMoveCapture("f1h1")
        |> should equal Stockfish.Capture.NO_CAPTURE

        stockfish.Value.WillMoveCapture("f1e1")
        |> should equal Stockfish.Capture.DIRECT_CAPTURE

        stockfish.Value.UpdateEngineParameters(readOnlyDict [ "UCI_Chess960", "false" ])

        stockfish.Value.GetParameters()
        |> TestUtil.dictEq oldParameters
        |> should be True

        stockfish.Value.GetBestMove() |> should equal (Some "f1g1")

        stockfish.Value.GetEvaluation()
        |> TestUtil.dictEq (readOnlyDict [ "type", "mate"; "value", 2 ])
        |> should be True

        stockfish.Value.WillMoveCapture("f1g1")
        |> should equal Stockfish.Capture.NO_CAPTURE

    [<Fact>]
    let ``GetBoardVisual as white`` () =
        stockfish.Value.SetPosition([ "e2e4"; "e7e6"; "d2d4"; "d7d5" ])
        let mutable expectedResult = ""

        if stockfish.Value.StockfishMajorVersion >= 12 then
            let board =
                seq {
                    "+---+---+---+---+---+---+---+---+"
                    "| r | n | b | q | k | b | n | r | 8"
                    "+---+---+---+---+---+---+---+---+"
                    "| p | p | p |   |   | p | p | p | 7"
                    "+---+---+---+---+---+---+---+---+"
                    "|   |   |   |   | p |   |   |   | 6"
                    "+---+---+---+---+---+---+---+---+"
                    "|   |   |   | p |   |   |   |   | 5"
                    "+---+---+---+---+---+---+---+---+"
                    "|   |   |   | P | P |   |   |   | 4"
                    "+---+---+---+---+---+---+---+---+"
                    "|   |   |   |   |   |   |   |   | 3"
                    "+---+---+---+---+---+---+---+---+"
                    "| P | P | P |   |   | P | P | P | 2"
                    "+---+---+---+---+---+---+---+---+"
                    "| R | N | B | Q | K | B | N | R | 1"
                    "+---+---+---+---+---+---+---+---+"
                    "  a   b   c   d   e   f   g   h\n"
                }
                |> fun s -> String.Join("\n", s)

            expectedResult <- board
        else
            let board =
                seq {
                    "+---+---+---+---+---+---+---+---+"
                    "| r | n | b | q | k | b | n | r |"
                    "+---+---+---+---+---+---+---+---+"
                    "| p | p | p |   |   | p | p | p |"
                    "+---+---+---+---+---+---+---+---+"
                    "|   |   |   |   | p |   |   |   |"
                    "+---+---+---+---+---+---+---+---+"
                    "|   |   |   | p |   |   |   |   |"
                    "+---+---+---+---+---+---+---+---+"
                    "|   |   |   | P | P |   |   |   |"
                    "+---+---+---+---+---+---+---+---+"
                    "|   |   |   |   |   |   |   |   |"
                    "+---+---+---+---+---+---+---+---+"
                    "| P | P | P |   |   | P | P | P |"
                    "+---+---+---+---+---+---+---+---+"
                    "| R | N | B | Q | K | B | N | R |"
                    "+---+---+---+---+---+---+---+---+"
                }
                |> fun s -> String.Join("\n", s)

            expectedResult <- board

        stockfish.Value.GetBoardVisual()
        |> should equal expectedResult

        stockfish.Value.Put("d")
        stockfish.Value.ReadLine() |> ignore // skip a line

        stockfish.Value.ReadLine().Contains "+---+---+---+"
        |> should be True
    // Tests that the previous call to get_board_visual left no remaining lines to be read. This means
    // the second line read after stockfish.Value._put("d") now will be the +---+---+---+ of the new outputted board.

    [<Fact>]
    let ``GetBoardVisual as black`` () =
        stockfish.Value.SetPosition([ "e2e4"; "e7e6"; "d2d4"; "d7d5" ])
        let mutable expectedResult = ""

        if stockfish.Value.StockfishMajorVersion >= 12 then
            expectedResult <-
                seq {
                    "+---+---+---+---+---+---+---+---+"
                    "| R | N | B | K | Q | B | N | R | 1"
                    "+---+---+---+---+---+---+---+---+"
                    "| P | P | P |   |   | P | P | P | 2"
                    "+---+---+---+---+---+---+---+---+"
                    "|   |   |   |   |   |   |   |   | 3"
                    "+---+---+---+---+---+---+---+---+"
                    "|   |   |   | P | P |   |   |   | 4"
                    "+---+---+---+---+---+---+---+---+"
                    "|   |   |   |   | p |   |   |   | 5"
                    "+---+---+---+---+---+---+---+---+"
                    "|   |   |   | p |   |   |   |   | 6"
                    "+---+---+---+---+---+---+---+---+"
                    "| p | p | p |   |   | p | p | p | 7"
                    "+---+---+---+---+---+---+---+---+"
                    "| r | n | b | k | q | b | n | r | 8"
                    "+---+---+---+---+---+---+---+---+"
                    "  h   g   f   e   d   c   b   a\n"
                }
                |> fun s -> String.Join("\n", s)
        else
            expectedResult <-
                seq {
                    "+---+---+---+---+---+---+---+---+"
                    "| R | N | B | K | Q | B | N | R |"
                    "+---+---+---+---+---+---+---+---+"
                    "| P | P | P |   |   | P | P | P |"
                    "+---+---+---+---+---+---+---+---+"
                    "|   |   |   |   |   |   |   |   |"
                    "+---+---+---+---+---+---+---+---+"
                    "|   |   |   | P | P |   |   |   |"
                    "+---+---+---+---+---+---+---+---+"
                    "|   |   |   |   | p |   |   |   |"
                    "+---+---+---+---+---+---+---+---+"
                    "|   |   |   | p |   |   |   |   |"
                    "+---+---+---+---+---+---+---+---+"
                    "| p | p | p |   |   | p | p | p |"
                    "+---+---+---+---+---+---+---+---+"
                    "| r | n | b | k | q | b | n | r |"
                    "+---+---+---+---+---+---+---+---+"
                }
                |> fun s -> String.Join("\n", s)

        stockfish.Value.GetBoardVisual(false)
        |> should equal expectedResult

        stockfish.Value.Put("d")
        stockfish.Value.ReadLine() |> ignore // skip a line

        stockfish.Value.ReadLine().Contains "+---+---+---+"
        |> should be True
    // Tests that the previous call to get_board_visual left no remaining lines to be read. This means
    // the second line read after stockfish.Value._put("d") now will be the +---+---+---+ of the new outputted board.

    [<Fact>]
    let ``GetFenPosition`` () =
        stockfish.Value.GetFenPosition()
        |> should equal "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

        stockfish.Value.Put("d")
        stockfish.Value.ReadLine() |> ignore // skip a line

        stockfish.Value.ReadLine().Contains "+---+---+---+"
        |> should be True

    [<Fact>]
    let ``GetFenPosition should update after some moves`` () =
        stockfish.Value.SetPosition([ "e2e4"; "e7e6" ])

        stockfish.Value.GetFenPosition()
        |> should equal "rnbqkbnr/pppp1ppp/4p3/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"

    [<Fact>]
    let ``StockfishMajorVersion`` () =
        //[ 8; 9; 10; 11; 12; 13; 14; 15 ]
        //|> Seq.contains (stockfish.Value.get_stockfish_major_version ())
        //<> stockfish.Value.is_development_build_of_engine ()
        stockfish.Value.StockfishMajorVersion
        |> should be (greaterThanOrEqualTo 16)

    [<Fact>]
    let ``GetEvaluation should have cp type`` () =
        stockfish.Value.SetDepth(20)
        stockfish.Value.SetFenPosition("r4rk1/pppb1p1p/2nbpqp1/8/3P4/3QBN2/PPP1BPPP/R4RK1 w - - 0 11")
        let evaluation = stockfish.Value.GetEvaluation()

        (evaluation.["type"] = "cp"
         && evaluation.["value"] :?> int >= 60
         && evaluation.["value"] :?> int <= 150)
        |> should be True

    [<Fact>]
    let ``GetEvaluation should have checkmate type`` () =
        stockfish.Value.SetFenPosition("1nb1k1n1/pppppppp/8/6r1/5bqK/6r1/8/8 w - - 2 2")
        let evaluation = stockfish.Value.GetEvaluation()
        evaluation.["type"] |> should equal "mate"
        evaluation.["value"] |> should equal 0

    [<Fact>]
    let ``GetEvaluation should have stalemate type`` () =
        stockfish.Value.SetFenPosition("1nb1kqn1/pppppppp/8/6r1/5b1K/6r1/8/8 w - - 2 2")
        let evaluation = stockfish.Value.GetEvaluation()
        evaluation.["type"] |> should equal "cp"
        evaluation.["value"] |> should equal 0

    [<Fact>]
    let ``SetDepth`` () =
        stockfish.Value.SetDepth(12)
        stockfish.Value.Depth |> should equal 12
        stockfish.Value.GetBestMove() |> ignore
        stockfish.Value.Info.Contains "depth 12" |> should be True

    [<Fact>]
    let ``GetBestMove wrong position`` () =
        stockfish.Value.SetDepth(2)
        let wrongFen = "3kk3/8/8/8/8/8/8/3KK3 w - - 0 0"
        stockfish.Value.SetFenPosition(wrongFen)

        [ "d1e2"; "d1c1"; "d1c2" ]
        |> Seq.map Some
        |> should contain (stockfish.Value.GetBestMove())

    [<Fact>]
    let ``Constructor stockfish parameters should be initialized`` () =
        // Will also use a new stockfish instance in order to test sending
        // params to the constructor.

        use stockfish2 =
            new Stockfish(
                EXE_PATH,
                depth = 16,
                parameters =
                    readOnlyDict [
                        "MultiPV", 2
                        "UCI_Elo", 2850
                        "UCI_Chess960", "true"
                    ]
            )

        stockfish2.GetFenPosition()
        |> should equal "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w HAha - 0 1"

        stockfish.Value.GetFenPosition()
        |> should equal "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

        stockfish2.GetBestMove() |> ignore
        stockfish.Value.GetBestMove() |> ignore
        stockfish2.Info.Contains "multipv 2" |> should be True
        stockfish2.Info.Contains "depth 16" |> should be True

        stockfish2.Depth |> should equal 16
        stockfish.Value.Info.Contains "multipv 1" |> should be True
        stockfish.Value.Info.Contains "depth 15" |> should be True
        stockfish.Value.Depth |> should equal 15

        let stockfish1Params = stockfish.Value.GetParameters()
        let stockfish2Params = stockfish2.GetParameters()

        for key in stockfish2Params.Keys do
            if key = "MultiPV" then
                stockfish2Params.[key] |> should equal 2
                stockfish1Params.[key] |> should equal 1
            elif key = "UCI_Elo" then
                stockfish2Params.[key] |> should equal 2850
                stockfish1Params.[key] |> should equal 1350
            elif key = "UCI_LimitStrength" then
                stockfish2Params.[key] |> should equal "true"
                stockfish1Params.[key] |> should equal "false"
            elif key = "UCI_Chess960" then
                stockfish2Params.[key] |> should equal "true"
                stockfish1Params.[key] |> should equal "false"
            else
                stockfish2Params.[key]
                |> should equal stockfish1Params.[key]

    [<Fact>]
    let ``General parameter function usage`` () =
        let oldParameters = stockfish.Value.GetParameters()
        stockfish.Value.SetFenPosition("4rkr1/4p1p1/8/8/8/8/8/5K1R w H - 0 100")

        stockfish.Value.GetBestMove() |> should equal (Some "f1g1") // ensures Chess960 param is false.

        stockfish.Value.GetFenPosition()
        |> should equal "4rkr1/4p1p1/8/8/8/8/8/5K1R w K - 0 100"

        stockfish.Value.Info.Contains "multipv 1" |> should be True

        stockfish.Value.UpdateEngineParameters(
            readOnlyDict [
                "Minimum Thinking Time", 10
                "Hash", 32
                "MultiPV", 2
                "UCI_Chess960", "true"
            ]
        )

        stockfish.Value.GetFenPosition()
        |> should equal "4rkr1/4p1p1/8/8/8/8/8/5K1R w H - 0 100"

        stockfish.Value.GetBestMove() |> should equal (Some "f1h1")

        stockfish.Value.Info.Contains "multipv 2" |> should be True
        let updatedParameters = stockfish.Value.GetParameters()

        for key, value in updatedParameters |> Seq.map (fun x -> x.Deconstruct()) do
            if key = "Minimum Thinking Time" then
                value |> should equal 10
            elif key = "Hash" then
                value |> should equal 32
            elif key = "MultiPV" then
                value |> should equal 2
            elif key = "UCI_Chess960" then
                value |> should equal "true"
            else
                updatedParameters.[key] |> should equal oldParameters.[key]

        stockfish.Value.GetParameters().["UCI_LimitStrength"]
        |> should equal "false"

        stockfish.Value.UpdateEngineParameters(readOnlyDict [ "UCI_Elo", 2000; "Skill Level", 19 ])

        stockfish.Value.GetParameters().["UCI_Elo"]
        |> should equal 2000

        stockfish.Value.GetParameters().["Skill Level"]
        |> should equal 19

        stockfish.Value.GetParameters().["UCI_LimitStrength"]
        |> should equal "false"

        stockfish.Value.UpdateEngineParameters(readOnlyDict [ "UCI_Elo", 2000 ])

        stockfish.Value.GetParameters().["UCI_LimitStrength"]
        |> should equal "true"

        stockfish.Value.UpdateEngineParameters(readOnlyDict [ "Skill Level", 20 ])

        stockfish.Value.GetParameters().["UCI_LimitStrength"]
        |> should equal "false"

        stockfish.Value.GetFenPosition()
        |> should equal "4rkr1/4p1p1/8/8/8/8/8/5K1R w H - 0 100"

        stockfish.Value.ResetEngineParameters()

        stockfish.Value.GetParameters()
        |> TestUtil.dictEq oldParameters
        |> should be True

        stockfish.Value.GetFenPosition()
        |> should equal "4rkr1/4p1p1/8/8/8/8/8/5K1R w K - 0 100"

        try
            stockfish.Value.UpdateEngineParameters(readOnlyDict [ "Not an existing key", "value" ])
        with
        | :? StockfishValueException -> ()
        | _ -> shouldFail (fun () -> printfn "should've thrown StockfishValueException")

    [<Fact>]
    let ``GetTopMoves with 2`` () =
        stockfish.Value.SetDepth(15)
        stockfish.Value.SetOption("MultiPV", 4)
        stockfish.Value.SetFenPosition("1rQ1r1k1/5ppp/8/8/1R6/8/2r2PPP/4R1K1 w - - 0 1")

        seq {
            let expected = [
                readOnlyDict [
                    "Move", "e1e8" :> obj
                    "Centipawn", None :> obj
                    "Mate", 1 :> obj
                ]
                readOnlyDict [
                    "Move", "c8e8" :> obj
                    "Centipawn", None :> obj
                    "Mate", 2 :> obj
                ]
            ]

            let actual = stockfish.Value.GetTopMoves(2) |> Seq.toList

            for i in 0 .. actual.Length - 1 do
                TestUtil.dictEq expected.[i] actual.[i]
        }
        |> Seq.forall id
        |> should be True

        stockfish.Value.SetFenPosition("8/8/8/8/8/3r2k1/8/6K1 w - - 0 1")

        seq {
            let expected = [
                readOnlyDict [
                    "Move", "g1f1" :> obj
                    "Centipawn", None :> obj
                    "Mate", -2 :> obj
                ]
                readOnlyDict [
                    "Move", "g1h1" :> obj
                    "Centipawn", None :> obj
                    "Mate", -1 :> obj
                ]
            ]

            let actual = stockfish.Value.GetTopMoves(2) |> Seq.toList

            for i in 0 .. actual.Length - 1 do
                TestUtil.dictEq expected.[i] actual.[i]
        }
        |> Seq.forall id
        |> should be True

    [<Fact>]
    let ``GetTopMoves while in checkmate`` () =
        stockfish.Value.SetDepth(10)
        stockfish.Value.SetOption("MultiPV", 3)
        stockfish.Value.SetFenPosition("8/8/8/8/8/6k1/8/3r2K1 w - - 0 1")
        stockfish.Value.GetTopMoves() |> should be Empty

        stockfish.Value.GetParameters().["MultiPV"]
        |> should equal 3

    [<Fact>]
    let ``GetTopMoves raising error`` () =
        stockfish.Value.SetFenPosition("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

        try
            stockfish.Value.GetTopMoves(0) |> ignore
        with
        | :? StockfishValueException -> ()
        | _ -> shouldFail (fun () -> printfn "should've thrown an StockfishValueException")

        stockfish.Value.GetTopMoves(2)
        |> Seq.length
        |> should equal 2

        stockfish.Value.GetParameters().["MultiPV"]
        |> should equal 1

    [<Fact>]
    let ``MakeMovesFromCurrentPosition`` () =
        stockfish.Value.SetFenPosition("r1bqkb1r/pppp1ppp/2n2n2/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 1")
        let fen1 = stockfish.Value.GetFenPosition()
        stockfish.Value.MakeMovesFromCurrentPosition([])
        fen1 |> should equal (stockfish.Value.GetFenPosition())

        stockfish.Value.MakeMovesFromCurrentPosition([ "e1g1" ])

        stockfish.Value.GetFenPosition()
        |> should equal "r1bqkb1r/pppp1ppp/2n2n2/1B2p3/4P3/5N2/PPPP1PPP/RNBQ1RK1 b kq - 1 1"

        stockfish.Value.MakeMovesFromCurrentPosition(
            [
                "f6e4"
                "d2d4"
                "e4d6"
                "b5c6"
                "d7c6"
                "d4e5"
                "d6f5"
            ]
        )

        stockfish.Value.GetFenPosition()
        |> should equal "r1bqkb1r/ppp2ppp/2p5/4Pn2/8/5N2/PPP2PPP/RNBQ1RK1 w kq - 1 5"

        stockfish.Value.MakeMovesFromCurrentPosition(
            [
                "d1d8"
                "e8d8"
                "b1c3"
                "d8e8"
                "f1d1"
                "f5e7"
                "h2h3"
                "f7f5"
            ]
        )

        stockfish.Value.GetFenPosition()
        |> should equal "r1b1kb1r/ppp1n1pp/2p5/4Pp2/8/2N2N1P/PPP2PP1/R1BR2K1 w - f6 0 9"

        stockfish.Value.SetFenPosition("r1bqk2r/pppp1ppp/8/8/1b2n3/2N5/PPP2PPP/R1BQK2R w Qkq - 0 1")

        let invalidMoves = [
            "d1e3"
            "e1g1"
            "c3d5"
            "c1d4"
            "a7a6"
            "e1d2"
            "word"
        ]

        for invalidMove in invalidMoves do
            try
                stockfish.Value.MakeMovesFromCurrentPosition([ invalidMove ])
            with
            | :? StockfishValueException -> ()
            | _ -> shouldFail (fun () -> printfn "should've thrown a StockfishValueException")

    ///<summary>
    /// make_moves_from_current_position won't send the "ucinewgame" token to Stockfish, since it
    /// will reach a new position similar to the current one. Meanwhile, set_fen_position will send this
    /// token (unless the user specifies otherwise), since it could be going to a completely new position.
    ///
    /// A big effect of sending this token is that it resets SF's transposition table. If the
    /// new position is similar to the current one, this will affect SF's speed. This function tests
    /// that make_moves_from_current_position doesn't reset the transposition table, by verifying SF is faster in
    /// evaluating a consecutive set of positions when the make_moves_from_current_position function is used.
    ///</summary>
    [<Fact>]
    let ``MakeMovesFromCurrentPosition transposition table speed`` () =
        stockfish.Value.SetDepth(16)
        let positionsConsidered = ResizeArray()
        stockfish.Value.SetFenPosition("rnbqkbnr/ppp1pppp/8/3p4/2PP4/8/PP2PPPP/RNBQKBNR b KQkq - 0 2")

        let defaultTimer () = TimeProvider.System.GetTimestamp()

        let mutable totalTimeCalculatingFirst = 0L

        for i in seq { 0 .. 5 - 1 } do
            let start = defaultTimer ()
            let chosenMove = stockfish.Value.GetBestMove()
            &totalTimeCalculatingFirst += (defaultTimer () - start)
            positionsConsidered.Add(stockfish.Value.GetFenPosition())
            stockfish.Value.MakeMovesFromCurrentPosition([ chosenMove.Value ])

        let mutable totalTimeCalculatingSecond = 0L

        for i in seq { 0 .. positionsConsidered.Count - 1 } do
            stockfish.Value.SetFenPosition(positionsConsidered.[i])
            let start = defaultTimer ()
            stockfish.Value.GetBestMove() |> ignore
            &totalTimeCalculatingSecond += (defaultTimer () - start)

        totalTimeCalculatingFirst
        |> should be (lessThan totalTimeCalculatingSecond)

    [<Fact>]
    let ``GetWdlStats`` () =
        stockfish.Value.SetDepth(15)
        stockfish.Value.SetOption("MultiPV", 2)

        if stockfish.Value.DoesEngineHaveWdlOption() then
            stockfish.Value.GetWdlStats() |> ignore // Testing that this doesn't raise a RuntimeError.
            stockfish.Value.SetFenPosition("7k/4R3/4P1pp/7N/8/8/1q5q/3K4 w - - 0 1")
            let wdlStats = stockfish.Value.GetWdlStats().Value
            wdlStats.[1] > wdlStats.[0] * 7 |> should be True

            float (abs (wdlStats.[0] - wdlStats.[2]) / wdlStats.[0]) < 0.1
            |> should be True

            stockfish.Value.SetFenPosition("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
            let wdlStats2 = stockfish.Value.GetWdlStats().Value

            float (wdlStats2.[1]) > float (wdlStats2.[0]) * 3.5
            |> should be True

            float (wdlStats2.[0]) > float (wdlStats2.[2]) * 1.8
            |> should be True

            stockfish.Value.SetFenPosition("8/8/8/8/8/6k1/6p1/6K1 w - - 0 1")
            stockfish.Value.GetWdlStats().IsNone |> should be True

            stockfish.Value.SetFenPosition("rnbqkb1r/pp3ppp/3p1n2/1B2p3/3NP3/2N5/PPP2PPP/R1BQK2R b KQkq - 0 6")

            stockfish.Value
                .GetWdlStats()
                .Value.Count
            |> should equal 3

            stockfish.Value.SetFenPosition("8/8/8/8/8/3k4/3p4/3K4 w - - 0 1")
            stockfish.Value.GetWdlStats().IsNone |> should be True
        else
            try
                stockfish.Value.GetWdlStats() |> ignore
            with
            | :? StockfishRuntimeException -> ()
            | _ -> shouldFail (fun () -> printfn "should've thrown a StockfishRuntimeException")

    [<Fact>]
    let ``DoesEngineHaveWdlOption`` () =
        if stockfish.Value.StockfishMajorVersion <= 11 then
            stockfish.Value.DoesEngineHaveWdlOption() |> should be False

            try
                stockfish.Value.GetWdlStats() |> ignore
            with
            | :? StockfishRuntimeException -> ()
            | _ -> shouldFail (fun () -> printfn "should've thrown a StockfishRuntimeException")
        else
            ()

    ///////// can't see how this cmd hangs... leaving this command out for now
    ////////[<Fact>]
    ////////let test_benchmark_result_with_defaults () =
    ////////    let parameters = Stockfish.BenchmarkParameters()
    ////////    let result = stockfish.Value.benchmark (parameters)  (stuck on read_line I think)
    ////////    // result should contain the last line of a successful method call
    ////////    result.split(" ").[0] |> should equal "Nodes/second"

    ////////[<Fact>]
    ////////let test_benchmark_result_with_valid_options () =
    ////////    let parameters =
    ////////        Stockfish.BenchmarkParameters(
    ////////            ttSize = 64,
    ////////            threads = 2,
    ////////            limit = 1000,
    ////////            limitType = "movetime",
    ////////            evalType = "classical"
    ////////        )

    ////////    let result = stockfish.Value.benchmark (parameters)  (stuck on read_line I think)
    ////////    // result should contain the last line of a successful method call
    ////////    result.split(" ").[0] |> should equal "Nodes/second"

    ////////[<Fact>]
    ////////let test_benchmark_result_with_invalid_options () =
    ////////    let parameters =
    ////////        Stockfish.BenchmarkParameters(
    ////////            ttSize = 2049,
    ////////            threads = 0,
    ////////            limit = 0,
    ////////            fenFile = "./fakefile.fen",
    ////////            limitType = "fghthtr",
    ////////            evalType = ""
    ////////        )

    ////////    let result = stockfish.Value.benchmark (parameters)  (stuck on read_line I think)
    ////////    // result should contain the last line of a successful method call
    ////////    result.split(" ").[0] |> should equal "Nodes/second"

    ////Not do able in a type safe language
    //[<Fact>]
    //let test_benchmark_result_with_invalid_type()=
    //    let parameters = {
    //        "ttSize", 16;
    //        "threads", 1;
    //        "limit", 13;
    //        "fenFile", "./fakefile.fen";
    //        "limitType", "depth";
    //        "evalType", "mixed";
    //    }
    //    let result = stockfish.Value.benchmark(params)
    //    // result should contain the last line of a successful method call
    //    result.split(" ").[0] |> should equal "Nodes/second"

    ////Not do able this due to access modifiers and let bindings in F# classes
    //[<Fact>]
    //let test_multiple_calls_to_del()=
    //    assert stockfish.Value._stockfish.Value.poll() is None
    //    assert not stockfish.Value._has_quit_command_been_sent
    //    stockfish.Value.__del__()
    //    assert stockfish.Value._stockfish.Value.poll() is not None
    //    assert stockfish.Value._has_quit_command_been_sent
    //    stockfish.Value.__del__()
    //    assert stockfish.Value._stockfish.Value.poll() is not None
    //    assert stockfish.Value._has_quit_command_been_sent

    ///Not do able this due to access modifiers and let bindings in F# classes
    //[<Fact>]
    //let test_multiple_quit_commands()=
    //    // Test multiple quit commands, and include a call to del too. All of
    //    // them should run without causing some Exception.
    //    assert stockfish.Value._stockfish.Value.poll() is None
    //    assert not stockfish.Value._has_quit_command_been_sent
    //    stockfish.Value._put("quit")
    //    assert stockfish.Value._has_quit_command_been_sent
    //    stockfish.Value._put("quit")
    //    assert stockfish.Value._has_quit_command_been_sent
    //    stockfish.Value.__del__()
    //    assert stockfish.Value._stockfish.Value.poll() is not None
    //    assert stockfish.Value._has_quit_command_been_sent
    //    stockfish.Value._put(f"go depth {10}")
    //    // Should do nothing, and change neither of the values below.
    //    assert stockfish.Value._stockfish.Value.poll() is not None
    //    assert stockfish.Value._has_quit_command_been_sent

    [<Fact>]
    let ``GetPieceOnSquare`` () =
        stockfish.Value.SetFenPosition("rnbq1rk1/ppp1ppbp/5np1/3pP3/8/BPN5/P1PP1PPP/R2QKBNR w KQ d6 0 6")

        stockfish.Value.GetPieceOnSquare("a1")
        |> should equal (Some Stockfish.Piece.WHITE_ROOK)

        stockfish.Value.GetPieceOnSquare("a8")
        |> should equal (Some Stockfish.Piece.BLACK_ROOK)

        stockfish.Value.GetPieceOnSquare("g8")
        |> should equal (Some Stockfish.Piece.BLACK_KING)

        stockfish.Value.GetPieceOnSquare("e1")
        |> should equal (Some Stockfish.Piece.WHITE_KING)

        stockfish.Value.GetPieceOnSquare("h2")
        |> should equal (Some Stockfish.Piece.WHITE_PAWN)

        stockfish.Value.GetPieceOnSquare("f8")
        |> should equal (Some Stockfish.Piece.BLACK_ROOK)

        stockfish.Value.GetPieceOnSquare("d6") |> should equal None

        stockfish.Value.GetPieceOnSquare("h7")
        |> should equal (Some Stockfish.Piece.BLACK_PAWN)

        stockfish.Value.GetPieceOnSquare("c3")
        |> should equal (Some Stockfish.Piece.WHITE_KNIGHT)

        stockfish.Value.GetPieceOnSquare("a3")
        |> should equal (Some Stockfish.Piece.WHITE_BISHOP)

        stockfish.Value.GetPieceOnSquare("h8") |> should equal None

        stockfish.Value.GetPieceOnSquare("d1")
        |> should equal (Some Stockfish.Piece.WHITE_QUEEN)

        stockfish.Value.GetPieceOnSquare("d4") |> should equal None

        stockfish.Value.GetPieceOnSquare("f6")
        |> should equal (Some Stockfish.Piece.BLACK_KNIGHT)

        stockfish.Value.GetPieceOnSquare("g7")
        |> should equal (Some Stockfish.Piece.BLACK_BISHOP)

        stockfish.Value.GetPieceOnSquare("d8")
        |> should equal (Some Stockfish.Piece.BLACK_QUEEN)

        try
            stockfish.Value.GetPieceOnSquare("i1") |> ignore
        with
        | :? StockfishValueException -> ()
        | _ -> shouldFail (fun () -> printfn "should've thrown a StockfishValueException")

        try
            stockfish.Value.GetPieceOnSquare("b9") |> ignore
        with
        | :? StockfishValueException -> ()
        | _ -> shouldFail (fun () -> printfn "should've thrown a StockfishValueException")

    [<Fact>]
    let ``GetPieceOnSquare 13 spots`` () =
        stockfish.Value.SetFenPosition("rnbq1rk1/ppp1ppbp/5np1/3pP3/8/BPN5/P1PP1PPP/R2QKBNR w KQ d6 0 6")

        let expectedEnumMembers = [
            Stockfish.Piece.WHITE_PAWN
            Stockfish.Piece.BLACK_PAWN
            Stockfish.Piece.WHITE_KNIGHT
            Stockfish.Piece.BLACK_KNIGHT
            Stockfish.Piece.WHITE_BISHOP
            Stockfish.Piece.BLACK_BISHOP
            Stockfish.Piece.WHITE_ROOK
            Stockfish.Piece.BLACK_ROOK
            Stockfish.Piece.WHITE_QUEEN
            Stockfish.Piece.BLACK_QUEEN
            Stockfish.Piece.WHITE_KING
            Stockfish.Piece.BLACK_KING
        ]

        let rows = [
            "a"
            "b"
            "c"
            "d"
            "e"
            "f"
            "g"
            "h"
        ]

        let cols = [
            "1"
            "2"
            "3"
            "4"
            "5"
            "6"
            "7"
            "8"
        ]

        for row in rows do
            for col in cols do
                let value = stockfish.Value.GetPieceOnSquare(row + col)

                (value.IsNone
                 || (expectedEnumMembers |> Seq.contains value.Value))
                |> should be True

    [<Fact>]
    let ``WillMoveCapture`` () =
        stockfish.Value.SetFenPosition("1nbq1rk1/Ppp1ppbp/5np1/3pP3/8/BPN5/P1PP1PPP/R2QKBNR w KQ d6 0 6")

        let c3d5 = stockfish.Value.WillMoveCapture("c3d5")
        c3d5 |> should equal Stockfish.Capture.DIRECT_CAPTURE
        c3d5.ToString() |> should equal "direct capture"

        let e5d6 = stockfish.Value.WillMoveCapture("e5d6")
        e5d6 |> should equal Stockfish.Capture.EN_PASSANT
        e5d6.ToString() |> should equal "en passant"

        let f1e2 = stockfish.Value.WillMoveCapture("f1e2")
        f1e2 |> should equal Stockfish.Capture.NO_CAPTURE
        f1e2.ToString() |> should equal "no capture"

        let e5f6 = stockfish.Value.WillMoveCapture("e5f6")
        e5f6 |> should equal Stockfish.Capture.DIRECT_CAPTURE
        e5f6.ToString() |> should equal "direct capture"

        let a3d6 = stockfish.Value.WillMoveCapture("a3d6")
        a3d6 |> should equal Stockfish.Capture.NO_CAPTURE
        a3d6.ToString() |> should equal "no capture"

        let a7a8q = stockfish.Value.WillMoveCapture("a7a8q")
        a7a8q |> should equal Stockfish.Capture.NO_CAPTURE
        a7a8q.ToString() |> should equal "no capture"

        let a7a8b = stockfish.Value.WillMoveCapture("a7a8b")
        a7a8b |> should equal Stockfish.Capture.NO_CAPTURE
        a7a8b.ToString() |> should equal "no capture"

        let a7b8q = stockfish.Value.WillMoveCapture("a7b8q")
        a7b8q |> should equal Stockfish.Capture.DIRECT_CAPTURE
        a7b8q.ToString() |> should equal "direct capture"

        let a7b8r = stockfish.Value.WillMoveCapture("a7b8r")
        a7b8r |> should equal Stockfish.Capture.DIRECT_CAPTURE
        a7b8r.ToString() |> should equal "direct capture"

        try
            stockfish.Value.WillMoveCapture("c3c5") |> ignore
        with
        | :? StockfishValueException -> ()
        | _ -> shouldFail (fun () -> printfn "should've thrown a StockfishValueException")

    [<MemberData(nameof StockfishTests.``Invalid fen king attacked - parameters``)>]
    [<Theory>]
    let ``Invalid fen king attacked`` (fens: string array) =
        for fen in fens do
            let stockfish = lazy new Stockfish(EXE_PATH)
            // Each of these FENs have correct syntax, but
            // involve a king being attacked while it's the opponent's turn.
            //let old_del_counter = Stockfish._del_counter
            Stockfish.IsFenSyntaxValid(fen) |> should be True

            if
                (fen = "8/8/8/3k4/3K4/8/8/8 b - - 0 1"
                 && stockfish.Value.StockfishMajorVersion >= 15)
            then
                // Since for that FEN, SF 15 actually outputs a best move without crashing (unlike SF 14 and earlier).
                ()
            else
                stockfish.Value.IsFenValid(fen) |> should be False
                //assert Stockfish._del_counter = old_del_counter + 2

                stockfish.Value.SetFenPosition(fen)

                try
                    stockfish.Value.GetEvaluation() |> ignore
                with
                | :? StockfishException -> ()
                | _ -> shouldFail (fun () -> printfn "should've thrown a StockfishException")

    [<Fact>]
    let ``IsFenValid`` () =
        let oldParams = stockfish.Value.GetParameters()
        let oldInfo = stockfish.Value.Info
        let oldDepth = stockfish.Value.Depth
        let oldFen = stockfish.Value.GetFenPosition()

        let correctFens = [
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
            "r1bQkb1r/ppp2ppp/2p5/4Pn2/8/5N2/PPP2PPP/RNB2RK1 b kq - 0 8"
            "4k3/8/4K3/8/8/8/8/8 w - - 10 50"
            "r1b1kb1r/ppp2ppp/3q4/8/P2Q4/8/1PP2PPP/RNB2RK1 w kq - 8 15"
        ]

        let invalidSyntaxFens = [
            "r1bQkb1r/ppp2ppp/2p5/4Pn2/8/5N2/PPP2PPP/RNB2RK b kq - 0 8" // passes regex... need to fix logic
            "rnbqkb1r/pppp1ppp/4pn2/8/2PP4/8/PP2PPPP/RNBQKBNR w KQkq - 3"
            "rn1q1rk1/pbppbppp/1p2pn2/8/2PP4/5NP1/PP2PPBP/RNBQ1RK1 w w - 5 7"
            "4k3/8/4K3/71/8/8/8/8 w - - 10 50" // passes regex... need to fix logic
        ]

        for correctFen, invalidSyntaxFen in Seq.zip correctFens invalidSyntaxFens do
            //let old_del_counter = Stockfish._del_counter
            stockfish.Value.IsFenValid(correctFen) |> should be True

            stockfish.Value.IsFenValid(invalidSyntaxFen)
            |> should be False

            Stockfish.IsFenSyntaxValid(correctFen) |> should be True

            Stockfish.IsFenSyntaxValid(invalidSyntaxFen)
            |> should be False
            //assert Stockfish._del_counter = old_del_counter + 2
            ()

        //time.sleep(2.0)
        Thread.Sleep(2)
        //stockfish.Value._stockfish.poll() is None
        stockfish.Value.GetParameters()
        |> TestUtil.dictEq oldParams
        |> should be True

        stockfish.Value.Info |> should equal oldInfo
        stockfish.Value.Depth |> should equal oldDepth
        stockfish.Value.GetFenPosition() |> should equal oldFen

    [<Fact>]
    let ``Quit`` () =
        //stockfish.Value._stockfish.Value.poll() is None
        //old_del_counter = Stockfish._del_counter
        stockfish.Value.Quit()
        //stockfish.Value._stockfish.Value.poll() is not None
        stockfish.Value.Dispose()
        //stockfish.Value._stockfish.Value.poll() is not None
        //Stockfish._del_counter = old_del_counter + 1
        ()

    static member ``GetBestMove should update "Info" - parameters``() : obj seq = [|
        [|
            [|
                "info"
                "depth"
                "seldepth"
                "multipv"
                "score"
                "mate"
                "-1"
                "nodes"
                "nps"
                "tbhits"
                "time"
                "pv"
                "h2g1"
                "h4g3"
            |]
        |]
    |]

    static member ``Invalid fen king attacked - parameters``() : obj seq = [|
        [|
            [|
                "2k2q2/8/8/8/8/8/8/2Q2K2 w - - 0 1"
                "8/8/8/3k4/3K4/8/8/8 b - - 0 1"
                "1q2nB2/pP1k2KP/NN1Q1qP1/8/1P1p4/4p1br/3R4/6n1 w - - 0 1"
                "3rk1n1/ppp3pp/8/8/8/8/PPP5/1KR1R3 w - - 0 1"
            |]
        |]
    |]

    interface IAsyncDisposable with
        member this.DisposeAsync() =
            task {
                if stockfish.IsValueCreated then
                    let task =
                        Task.Run(fun () ->
                            stockfish.Value.Quit()
                            stockfish.Value.Dispose())

                    do! task.WaitAsync(TimeSpan.FromSeconds 2)
            }
            |> ValueTask
