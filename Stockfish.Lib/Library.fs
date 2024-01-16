module Stockfish.Lib

// PORTED FROM - https://github.com/zhelyabuzhsky/stockfish/blob/master/stockfish/models.py
// UCI INFO - https://backscattering.de/chess/uci/
// DOCS - https://disservin.github.io/stockfish-docs/pages/Home.html
// CMDS - https://gist.github.com/aliostad/f4470274f39d29b788c1b09519e67372
// FENSTRING REGEX - https://regex101.com/r/bA8eQ1/1

open System
open System.Text
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Diagnostics

[<assembly: InternalsVisibleTo("Stockfish.Lib.Test.Integration")>]
do ()

[<AutoOpen>]
module internal Util =
    type Process with

        member this.IsRunning() = this.Responding && not this.HasExited

    type IDictionary<'a, 'b> with

        member this.AddOrUpdate(k: 'a, v: 'b) : unit =
            match (this.IsReadOnly, this.ContainsKey k) with
            | (true, _) -> ()
            | (false, true) -> this.[k] <- v
            | (false, false) -> this.Add(k, v)

    let inline increment<'a when 'a: (static member (+): 'a * 'a -> 'a)> (x: byref<'a>) (v: 'a) = x <- x + v
    let inline (+=) (x: byref<_>) (v: _) = increment &x v

type StockfishException =
    inherit exn
    new(msg: string, inner: exn) = { inherit exn(msg, inner) }
    new(msg: string) = { inherit exn(msg) }
    new() = { inherit exn() }

type StockfishValueException =
    inherit StockfishException

    new(msg: string, inner: exn) =
        {
            inherit StockfishException(msg, inner)
        }

    new(msg: string) = { inherit StockfishException(msg) }
    new() = { inherit StockfishException() }

type StockfishBrokenPipeException =
    inherit StockfishException

    new(msg: string, inner: exn) =
        {
            inherit StockfishException(msg, inner)
        }

    new(msg: string) = { inherit StockfishException(msg) }
    new() = { inherit StockfishException() }

type StockfishRuntimeException =
    inherit StockfishException

    new(msg: string, inner: exn) =
        {
            inherit StockfishException(msg, inner)
        }

    new(msg: string) = { inherit StockfishException(msg) }
    new() = { inherit StockfishException() }

module Stockfish =
    type Piece =
        | WHITE_PAWN = 'P'
        | BLACK_PAWN = 'p'
        | WHITE_KNIGHT = 'N'
        | BLACK_KNIGHT = 'n'
        | WHITE_BISHOP = 'B'
        | BLACK_BISHOP = 'b'
        | WHITE_ROOK = 'R'
        | BLACK_ROOK = 'r'
        | WHITE_QUEEN = 'Q'
        | BLACK_QUEEN = 'q'
        | WHITE_KING = 'K'
        | BLACK_KING = 'k'

    module Piece =
        let value: Piece -> char = Convert.ToChar

        let internal _pieces = lazy Enum.GetValues<Piece>()
        let internal _vals = lazy (_pieces.Value |> Array.map value)

        let internal _pieceToCharMap =
            lazy (Seq.zip _pieces.Value _vals.Value |> Map)

        let internal _charToPieceMap =
            lazy (Seq.zip _vals.Value _pieces.Value |> Map)

        let parse: char -> Piece =
            let get map k = Map.find k map
            get _charToPieceMap.Value

        let tryParse: char -> Piece option =
            let get map k = Map.tryFind k map
            get _charToPieceMap.Value

    type Capture =
        | DIRECT_CAPTURE
        | EN_PASSANT
        | NO_CAPTURE

        override this.ToString() =
            match this with
            | DIRECT_CAPTURE -> "direct capture"
            | EN_PASSANT -> "en passant"
            | NO_CAPTURE -> "no capture"

    module Fen =
        module Parser =
            let tryParse (input: string) : string array option =
                let splitOpts =
                    StringSplitOptions.RemoveEmptyEntries
                    ||| StringSplitOptions.TrimEntries

                if String.IsNullOrWhiteSpace(input) then
                    None
                else
                    let fenParts = input.Split(" ", splitOpts)
                    Some fenParts

            let parse = tryParse >> Option.get


        module Validator =
            open System.Text.RegularExpressions

            module Patterns =
                let generalFenRegex =
                    Regex(
                        @"^(?<PiecePlacement>((?<RankItem>[pnbrqkPNBRQK1-8]{1,8})\/?){8})\s+(?<SideToMove>b|w)\s+(?<Castling>-|K?Q?k?q)\s+(?<EnPassant>-|[a-h][3-6])\s+(?<HalfMoveClock>\d+)\s+(?<FullMoveNumber>\d+)\s*$",
                        RegexOptions.IgnorePatternWhitespace
                        ||| RegexOptions.Compiled
                    )

                let rankContinuousNumbers =
                    Regex(@"\d{2}", RegexOptions.Compiled)

                let rankValidCharacters =
                    Regex(@"[1-8]|[pkqbnrPKQBNR]", RegexOptions.Compiled)

                let validSideToMove = Regex(@"^(w|b)$", RegexOptions.Compiled)

                let validCastlingAbility =
                    Regex(@"^-$|^(KQ?k?q?|Qk?q?|kq?|q)$", RegexOptions.Compiled)

                let validEnPassantTarget =
                    Regex(@"^(-|[a-h][36])$", RegexOptions.Compiled)

                let validHalfMoveClock =
                    Regex(@"^([0-9]|[1-9][0-9])$", RegexOptions.Compiled)

                let validFullMoveCounter =
                    Regex(@"^([1-9][0-9]{0,1})$", RegexOptions.Compiled)

                let validateRegex (regex: Regex) (input: string) : bool = regex.IsMatch(input)

            let validateRank (notation: string) =
                let hasContinuousNumbers =
                    Patterns.rankContinuousNumbers.IsMatch(notation)

                let characters = notation.ToCharArray() |> Seq.map string

                let hasOnlyValidCharacters =
                    characters
                    |> Seq.exists (fun c -> Patterns.rankValidCharacters.IsMatch(c) |> not)
                    |> not

                let totalSquares =
                    characters
                    |> Seq.fold
                        (fun total c ->
                            let (isInteger, parsedLetter) = Int32.TryParse(c)
                            if isInteger then (total + parsedLetter) else (total + 1))
                        0

                hasOnlyValidCharacters
                && not hasContinuousNumbers
                && totalSquares = 8

            let validatePiecePlacement (notation: string) =
                let splitOpts =
                    StringSplitOptions.RemoveEmptyEntries
                    ||| StringSplitOptions.TrimEntries

                let ranks = notation.Split("/", splitOpts)

                if (ranks.Length <> 8) then
                    false
                else
                    ranks
                    |> Seq.fold (fun lastVal rank -> lastVal && validateRank (rank)) true

            let validateSideToMove =
                Patterns.validateRegex Patterns.validSideToMove

            let validateCastlingAbility =
                Patterns.validateRegex Patterns.validCastlingAbility

            let validateEnPassantTarget =
                Patterns.validateRegex Patterns.validEnPassantTarget

            let validateHalfMoveClock =
                Patterns.validateRegex Patterns.validHalfMoveClock

            let validateFullMoveCounter =
                Patterns.validateRegex Patterns.validFullMoveCounter

            let validate (fenParts: string array) =
                match fenParts with
                | [| piecePlacement; sideToMove; castlingAbility; enPassantTarget; halfMoveClock; fullMoveCounter |] ->
                    validatePiecePlacement (piecePlacement)
                    && validateSideToMove (sideToMove)
                    && validateCastlingAbility (castlingAbility)
                    && validateEnPassantTarget (enPassantTarget)
                    && validateHalfMoveClock (halfMoveClock)
                    && validateFullMoveCounter (fullMoveCounter)
                | _ -> false


type Stockfish(?path: string, ?depth: int, ?parameters: IReadOnlyDictionary<string, obj>) as this =
    let path =
        Option.defaultValue
            (if OperatingSystem.IsWindows() then
                 "stockfish.exe"
             else
                 "stockfish")
            path

    let mutable _depth = Option.defaultValue 15 depth

    let _DEFAULT_STOCKFISH_PARAMS =
        readOnlyDict<string, obj> [
            ("Debug Log File", "")
            ("Contempt", 0)
            ("Min Split Depth", 0)
            ("Threads", 1)
            ("Ponder", "false")
            ("Hash", 16)
            ("MultiPV", 1)
            ("Skill Level", 20)
            ("Move Overhead", 10)
            ("Minimum Thinking Time", 20)
            ("Slow Mover", 100)
            ("UCI_Chess960", "false")
            ("UCI_LimitStrength", "false")
            ("UCI_Elo", 1350)
        ]

    let _stockfish =
        ProcessStartInfo(
            path,
            RedirectStandardInput = true,
            StandardInputEncoding = Encoding.UTF8,
            RedirectStandardOutput = true,
            StandardOutputEncoding = Encoding.UTF8,
            RedirectStandardError = true,
            StandardErrorEncoding = Encoding.UTF8,
            CreateNoWindow = false,
            LoadUserProfile = false,
            ErrorDialog = false,
            UseShellExecute = false
        )
        |> Process.Start

    let mutable _hasQuitCommandBeenSent = false

    let _stockfishMajorVersion =
        _stockfish.StandardOutput.ReadLine()
        |> fun s ->
            s.Split(
                ' ',
                StringSplitOptions.RemoveEmptyEntries
                ||| StringSplitOptions.TrimEntries
            )
        |> Seq.item 1
        |> fun s ->
            s.Split(
                '.',
                StringSplitOptions.RemoveEmptyEntries
                ||| StringSplitOptions.TrimEntries
            )
        |> Seq.item 0
        |> fun s -> s.Replace("-", "")
        |> int

    //// DON'T TOUCH THIS MAGIC CODE
    do _stockfish.StandardInput.WriteLine("isready")
    do _stockfish.StandardInput.Flush()
    do _stockfish.StandardOutput.ReadLine() |> ignore
    do _stockfish.StandardInput.WriteLine("isready")
    do _stockfish.StandardInput.Flush()
    do _stockfish.StandardOutput.ReadLine() |> ignore
    //// DON'T TOUCH THIS MAGIC CODE


    let mutable _info = ""

    let _parameters = Dictionary<string, obj>()
    do this.UpdateEngineParameters(_DEFAULT_STOCKFISH_PARAMS)

    do
        match parameters with
        | Some parameters -> this.UpdateEngineParameters(parameters)
        | _ -> ()

    do
        if this.DoesEngineHaveWdlOption() then
            this.SetOption("UCI_ShowWDL", "true", false)

    do this.PrepareForNewPosition(true)


    static member internal IsFenSyntaxValid(fen: string) : bool =
        match Stockfish.Fen.Parser.tryParse fen with
        | Some(fenParts) -> Stockfish.Fen.Validator.validate fenParts
        | None -> false


    member this.Info = _info

    member this.Depth = _depth

    /// <summary>
    /// Returns Stockfish engine major version.
    /// </summary>
    member this.StockfishMajorVersion = _stockfishMajorVersion

    member this.GetParameters() =
        _parameters
        |> Seq.map (fun x -> x.Deconstruct())
        |> readOnlyDict

    member this.UpdateEngineParameters(?newParamValues: IReadOnlyDictionary<string, obj>) : unit =
        if Option.isNone newParamValues then
            ()
        else
            let inline in' k (d: IDictionary<string, obj>) = d.ContainsKey k
            let inline notIn k (d: IDictionary<string, obj>) = d.ContainsKey k |> not
            let (<->) = notIn
            let (>-<) = in'

            let mutable newParamValues =
                Dictionary<string, obj>(newParamValues.Value)

            if _parameters.Count > 0 then
                for (key, _) in newParamValues |> Seq.map (fun x -> x.Deconstruct()) do
                    if key <-> _parameters then
                        raise
                        <| StockfishValueException($"'%s{key}' is not a key that exists.")

            if
                ("Skill Level" >-< newParamValues)
                <> ("UCI_Elo" >-< newParamValues)
                && ("UCI_LimitStrength" <-> newParamValues)
            then
                // This means the user wants to update the Skill Level or UCI_Elo (only one,
                // not both), and that they didn't specify a new value for UCI_LimitStrength.
                // So, update UCI_LimitStrength, in case it's not the right value currently.
                if "Skill Level" >-< newParamValues then
                    newParamValues.AddOrUpdate("UCI_LimitStrength", "false")
                elif "UCI_Elo" >-< newParamValues then
                    newParamValues.AddOrUpdate("UCI_LimitStrength", "true")


            if "Threads" >-< newParamValues then
                // Recommended to set the hash param after threads.
                let threadsValue = newParamValues.["Threads"]
                newParamValues.Remove "Threads" |> ignore

                let mutable hashValue = None

                if "Hash" >-< newParamValues then
                    hashValue <- Some newParamValues.["Hash"]
                    newParamValues.Remove "Hash" |> ignore
                else
                    hashValue <- Some _parameters.["Hash"]

                newParamValues.["Threads"] <- threadsValue
                newParamValues.["Hash"] <- hashValue |> Option.toObj


            for name, value in newParamValues |> Seq.map (fun x -> x.Deconstruct()) do
                this.SetOption(name, value, true)


            this.SetFenPosition(this.GetFenPosition(), false)
    //      Getting SF to set the position again, since UCI option(s) have been updated.

    ///Resets the stockfish parameters.
    member this.ResetEngineParameters() : unit =
        this.UpdateEngineParameters(_DEFAULT_STOCKFISH_PARAMS)

    member internal this.PrepareForNewPosition(?sendUciNewGameToken: bool) : unit =
        let sendUciNewGameToken =
            Option.defaultValue true sendUciNewGameToken

        if sendUciNewGameToken then
            this.Put("ucinewgame")

        this.IsReady()
        _info <- ""

    member internal this.Put(command: string) : unit =
        if isNull _stockfish.StandardInput then
            raise <| StockfishBrokenPipeException()

        if _stockfish.IsRunning() && not _hasQuitCommandBeenSent then
            _stockfish.StandardInput.WriteLine(command)
            Debug.WriteLine($"CMD: %s{command}")
            _stockfish.StandardInput.Flush()

            if command = "quit" then
                _hasQuitCommandBeenSent <- true

    member internal this.ReadLine() =
        if isNull _stockfish.StandardOutput then
            raise <| StockfishBrokenPipeException()

        if _stockfish.IsRunning() |> not then
            raise
            <| StockfishException(
                $"The Stockfish process has crashed/exited (%i{_stockfish.ExitCode}) [%A{_stockfish.ExitTime}]"
            )

        let line = _stockfish.StandardOutput.ReadLine()

        if isNull line then
            ""
        else
            let line = line.Trim()
            Debug.WriteLine($"OUTPUT: %s{line}")
            line

    member internal this.SetOption(name: string, value: obj, ?updateParametersAttribute: bool) : unit =
        let updateParametersAttribute =
            Option.defaultValue true updateParametersAttribute

        this.Put($"setoption name %s{name} value {value}")

        if updateParametersAttribute then
            _parameters.AddOrUpdate(name, value)

        this.IsReady()

    member internal this.IsReady() : unit =
        this.Put("isready")

        while this.ReadLine() <> "readyok" do
            ()

    member internal this.Go() : unit = this.Put($"go depth %i{this.Depth}")

    member internal this.GoTime(time: int) : unit = this.Put($"go movetime %i{time}")

    member internal this.GoTime(time: TimeSpan) : unit =
        this.Put($"go movetime %f{time.TotalMilliseconds}")

    member internal this.GoRemainingTime(?wtime: int, ?btime: int) : unit =
        let mutable cmd = new StringBuilder("go")

        if not wtime.IsNone then
            cmd <- cmd.Append(" wtime ")
            cmd <- cmd.Append(wtime.Value)

        if not btime.IsNone then
            cmd <- cmd.Append(" btime ")
            cmd <- cmd.Append(btime.Value)

        this.Put(cmd.ToString())

    /// <summary>Returns current board position in Forsyth–Edwards notation (FEN).</summary>
    /// <returns>String with current position in Forsyth–Edwards notation (FEN)</returns>
    member this.GetFenPosition() : string =
        this.Put("d")
        let inline notIn (search: string) (str: string) = str.Contains search |> not
        let mutable result = ""
        let mutable continue' = true

        while continue' do
            let text = this.ReadLine()

            let splittedText =
                text.Split(
                    ' ',
                    StringSplitOptions.RemoveEmptyEntries
                    ||| StringSplitOptions.TrimEntries
                )

            if splittedText.Length > 0 && splittedText.[0] = "Fen:" then
                while notIn "Checkers" <| this.ReadLine() do
                    ()

                continue' <- false
                result <- String.Join(" ", splittedText.[1..])

        result

    /// <summary>
    /// Sets current board position in Forsyth–Edwards notation (FEN).
    /// </summary>
    /// <param name="fenPosition">FEN string of board position.</param>
    /// <param name="sendUciNewGameToken">
    /// Whether to send the "ucinewgame" token to the Stockfish engine.
    /// The most prominent effect this will have is clearing Stockfish's transposition table, which should be done if the new position is unrelated to the current position.
    /// </param>
    member this.SetFenPosition(fenPosition: string, ?sendUciNewGameToken: bool) : unit =
        let sendUciNewGameToken =
            Option.defaultValue true sendUciNewGameToken

        this.PrepareForNewPosition(sendUciNewGameToken)
        this.Put($"position fen %s{fenPosition}")

    /// <summary>
    /// Sets current board position.
    /// </summary>
    /// <param name="moves">
    /// A list of moves to set this position on the board.
    ///     Must be in full algebraic notation.
    ///     example: ['e2e4', 'e7e5']
    /// </param>
    member this.SetPosition(?moves: string seq) : unit =
        this.SetFenPosition("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", true)

        if Option.isSome moves then
            this.MakeMovesFromCurrentPosition(moves.Value)

    /// <summary>
    /// Sets a new position by playing the moves from the current position.
    /// </summary>
    /// <param name="moves">
    /// A list of moves to play in the current position, in order to reach a new position.
    ///     Must be in full algebraic notation.
    ///     Example: ["g4d7", "a8b8", "f1d1"]
    /// </param>
    member this.MakeMovesFromCurrentPosition(?moves: string seq) : unit =
        if moves.IsSome then
            let moves = moves.Value
            this.PrepareForNewPosition(false)

            for move in moves do
                if not <| this.IsMoveCorrect(move) then
                    raise
                    <| StockfishValueException($"Cannot make move: %s{move}")

                this.Put($"position fen %s{this.GetFenPosition()} moves %s{move}")

    /// <summary>
    /// Returns a visual representation of the current board position.
    /// </summary>
    /// <param name="perspectiveWhite">A bool that indicates whether the board should be displayed from the perspective of white (True: white, False: black)</param>
    /// <returns>String of visual representation of the chessboard with its pieces in current position.</returns>
    member this.GetBoardVisual(?perspectiveWhite: bool) : string =
        let perspectiveWhite = Option.defaultValue true perspectiveWhite

        this.Put("d")
        let boardRepLines = ResizeArray()
        let mutable countLines = 0

        while countLines < 17 do
            let boardStr = this.ReadLine()

            if boardStr.Contains "+" || boardStr.Contains "|" then
                &countLines += 1

                if perspectiveWhite then
                    boardRepLines.Add($"%s{boardStr}")
                else
                    // If the board is to be shown from black's point of view, all lines are
                    // inverted horizontally and at the end the order of the lines is reversed.
                    let boardPart = boardStr.[.. 33 - 1]
                    // To keep the displayed numbers on the right side,
                    // only the string representing the board is flipped.
                    let numberPart =
                        if boardStr.Length > 33 then boardStr.[33..] else ""

                    let revBoardPart =
                        boardPart |> Seq.rev |> (fun x -> String.Join("", x))

                    boardRepLines.Add($"%s{revBoardPart}%s{numberPart}")

        if not perspectiveWhite then
            boardRepLines.Reverse()

        let boardStr = this.ReadLine()

        if boardStr.Contains "a   b   c" then
            // Engine being used is recent enough to have coordinates, so add them:
            if perspectiveWhite then
                boardRepLines.Add($"  %s{boardStr}")
            else
                let revBoardStr =
                    boardStr |> Seq.rev |> (fun x -> String.Join("", x))

                boardRepLines.Add($"  %s{revBoardStr}")

        while this.ReadLine().Contains "Checkers" |> not do
            // Gets rid of the remaining lines in _stockfish.stdout.
            // "Checkers" is in the last line outputted by Stockfish for the "d" command.
            ()

        let boardRep = String.Join("\n", boardRepLines) + "\n"

        boardRep

    /// <summary>
    /// Sets current skill level of stockfish engine.
    /// </summary>
    /// <param name="skillLevel">Skill Level option between 0 (weakest level) and 20 (full strength)</param>
    member this.SetSkillLevel(?skillLevel: int) : unit =
        let skillLevel = Option.defaultValue 20 skillLevel

        this.UpdateEngineParameters(
            readOnlyDict [
                "UCI_LimitStrength", "false"
                "Skill Level", skillLevel
            ]
        )

    /// <summary>
    /// Sets current elo rating of stockfish engine, ignoring skill level.
    /// </summary>
    /// <param name="eloRating">Aim for an engine strength of the given Elo</param>
    member this.SetEloRating(?eloRating: int) : unit =
        let eloRating = Option.defaultValue 1350 eloRating

        this.UpdateEngineParameters(
            readOnlyDict [
                "UCI_LimitStrength", "true"
                "UCI_Elo", eloRating
            ]
        )

    /// <summary>
    /// Returns best move with current position on the board. wtime and btime arguments influence the search only if provided.
    /// </summary>
    /// <param name="wtime"></param>
    /// <param name="btime"></param>
    /// <returns>A string of move in algebraic notation or None, if it's a mate now.</returns>
    member this.GetBestMove(?wtime: int, ?btime: int) : string option =
        match (wtime, btime) with
        | (Some w, Some b) -> this.GoRemainingTime(w, b)
        | (None, Some b) -> this.GoRemainingTime(btime = b)
        | (Some w, None) -> this.GoRemainingTime(wtime = w)
        | (None, None) -> this.Go()

        this.GetBestMoveFromCurrentStockfishGoProcess()

    /// <summary>
    /// Returns best move with current position on the board after a determined time
    /// </summary>
    /// <param name="time">Time for stockfish to determine best move in milliseconds (int)</param>
    /// <returns>A string of move in algebraic notation or None, if it's a mate now.</returns>
    member this.GetBestMoveTime(?time: int) : string option =
        let time = Option.defaultValue 1000 time
        this.GoTime(time)
        this.GetBestMoveFromCurrentStockfishGoProcess()


    /// Precondition - a "go" command must have been sent to SF before calling this function.
    /// This function needs existing output to read from the SF popen process.
    member internal this.GetBestMoveFromCurrentStockfishGoProcess() : string option =
        let mutable lastText: string = ""
        let mutable result = None
        let mutable continue' = true

        while continue' do
            let text = this.ReadLine()
            let splittedText = text.Split(" ")

            if splittedText.[0] = "bestmove" then
                _info <- lastText
                continue' <- false

                result <-
                    if splittedText.[1] = "(none)" then
                        None
                    else
                        Some splittedText.[1]

            lastText <- text

        result

    member this.IsFenValid(fen: string) : bool =
        if Stockfish.IsFenSyntaxValid(fen) |> not then
            false
        else
            use tempSf =
                new Stockfish(path = path, parameters = readOnlyDict [ "Hash", 1 ])

            // Using a new temporary SF instance, in case the fen is an illegal position that causes
            // the SF process to crash.
            let mutable bestMove = None
            tempSf.SetFenPosition(fen, false)

            try
                tempSf.Put("go depth 10")
                bestMove <- tempSf.GetBestMoveFromCurrentStockfishGoProcess()
                not bestMove.IsNone
            with
            // If a StockfishException is thrown, then it happened in read_line() since the SF process crashed.
            // This is likely due to the position being illegal, so set the var to false:
            | :? StockfishException ->
                false

    /// <summary>
    /// Checks a new move to see if it is a valid chess move or not.
    /// </summary>
    /// <param name="moveValue">New move value in algebraic notation to check if it is a valid move.</param>
    /// <returns>True, if new move is valid, else False.</returns>
    member this.IsMoveValid(moveValue: string) : bool =
        let oldThisInfo = this.Info
        let oldFenPosition = this.GetFenPosition()

        try
            this.MakeMovesFromCurrentPosition(seq { moveValue })
            let newFenPosition = this.GetFenPosition()
            this.SetFenPosition(oldFenPosition)
            _info <- oldThisInfo
            newFenPosition <> oldFenPosition
        with
        | :? StockfishValueException as ex when ex.Message = $"Cannot make move: %s{moveValue}" -> false
        | _ -> false

    /// <summary>
    /// Checks new move.
    /// </summary>
    /// <param name="moveValue">New move value in algebraic notation.</param>
    /// <returns>True, if new move is correct, else False.</returns>
    member this.IsMoveCorrect(moveValue: string) : bool =
        let oldThisInfo = this.Info
        this.Put($"go depth 1 searchmoves %s{moveValue}")

        let isMoveCorrect =
            this.GetBestMoveFromCurrentStockfishGoProcess()
            |> Option.isNone
            |> not

        _info <- oldThisInfo
        isMoveCorrect

    /// <summary>
    /// Returns Stockfish's win/draw/loss stats for the side to move.
    /// </summary>
    /// <returns>A list of three integers, unless the game is over (in which case, None is returned).</returns>
    member this.GetWdlStats() : int ResizeArray option =
        if this.DoesEngineHaveWdlOption() |> not then
            raise
            <| StockfishRuntimeException(
                "Your version of Stockfish isn't recent enough to have the UCI_ShowWDL option."
            )

        this.Go()
        let (>-<) value seq = Seq.contains value seq
        let lines = ResizeArray()
        let mutable continue' = true

        while continue' do
            let text = this.ReadLine()
            let splittedText = text.Split(" ")
            lines.Add(splittedText)

            if splittedText.[0] = "bestmove" then
                continue' <- false

        let mutable i = 0
        let mutable continue' = true
        let mutable result: int ResizeArray option option = None
        let lines = lines |> Seq.rev |> ResizeArray

        while continue' do
            let currentLine = lines.[i]
            &i += 1
            continue' <- i <> lines.Count

            if currentLine.[0] = "bestmove" && currentLine.[1] = "(none)" then
                result <- Some None
                continue' <- false
            elif "multipv" >-< currentLine then
                let indexOfMultipv =
                    currentLine |> Seq.findIndex ((=) "multipv")

                if
                    currentLine.[indexOfMultipv + 1] = "1"
                    && "wdl" >-< currentLine
                then
                    let indexOfWdl = currentLine |> Seq.findIndex ((=) "wdl")
                    let wdlStats = ResizeArray()

                    for i in seq { 1 .. 4 - 1 } do
                        let value = currentLine.[indexOfWdl + i]
                        wdlStats.Add(int (value))

                    result <- Some <| Some wdlStats
                    continue' <- false

        if result.IsNone then
            raise
            <| StockfishRuntimeException("Reached the end of the GetWdlStats function.")
        else
            result.Value

    ///<summary>Returns whether the user's version of Stockfish has the option to display WDL stats.</summary>
    ///<returns>True, if SF has the option -- False otherwise.</returns>
    member this.DoesEngineHaveWdlOption() : bool =
        this.Put("uci")
        let mutable encounteredUciShowWDL = false
        let mutable continue' = true

        while continue' do
            let text = this.ReadLine()

            let splittedText =
                text.Split(
                    ' ',
                    StringSplitOptions.RemoveEmptyEntries
                    ||| StringSplitOptions.TrimEntries
                )

            if splittedText.Length > 0 && splittedText.[0] = "uciok" then
                continue' <- false
            elif Array.contains "UCI_ShowWDL" splittedText then
                encounteredUciShowWDL <- true
        //      Not returning right away, since the remaining lines should be read and
        //      discarded. So continue the loop until reaching "uciok", which is
        //      the last line SF outputs for the "uci" command.
        encounteredUciShowWDL

    /// <summary>
    /// Evaluates current position
    /// </summary>
    /// <returns>A dictionary of the current advantage with "type" as "cp" (centipawns) or "mate" (checkmate in)</returns>
    member this.GetEvaluation() : IReadOnlyDictionary<string, obj> =
        let mutable evaluation = readOnlyDict []
        let fenPosition = this.GetFenPosition()
        let compare = if fenPosition.Contains "w" then 1 else -1
        // Stockfish shows advantage relative to current player. This function will instead
        // use positive to represent advantage white, and negative for advantage black.
        this.Put($"position %s{fenPosition}")
        this.Go()
        let mutable continue' = true

        while continue' do
            let text = this.ReadLine()
            let splittedText = text.Split(" ")

            if splittedText.[0] = "info" then
                for n in seq { 0 .. splittedText.Length - 1 } do
                    if splittedText.[n] = "score" then
                        evaluation <-
                            readOnlyDict<string, obj> [
                                "type", splittedText.[n + 1]
                                "value", (int (splittedText.[n + 2]) * compare) :> obj
                            ]
            elif splittedText.[0] = "bestmove" then
                continue' <- false

        evaluation

    /// <summary>
    /// Returns info on the top moves in the position.
    /// </summary>
    /// <param name="numTopMoves">The number of moves to return info on, assuming there are at least those many legal moves.</param>
    /// <returns>A list of dictionaries. In each dictionary, there are keys for Move, Centipawn, and Mate; the corresponding value for either the Centipawn or Mate key will be None. If there are no moves in the position, an empty list is returned.</returns>
    member this.GetTopMoves(?numTopMoves: int) : IReadOnlyDictionary<string, obj> seq =
        let numTopMoves = Option.defaultValue 5 numTopMoves

        let (>-<) = Seq.contains
        let (<->) v s = Seq.contains v s |> not

        if numTopMoves <= 0 then
            raise
            <| StockfishValueException("numTopMoves is not a positive number.")

        let oldMultipvValue = _parameters.["MultiPV"]

        if numTopMoves <> (_parameters.["MultiPV"] :?> int) then
            this.SetOption("MultiPV", numTopMoves)
            _parameters.AddOrUpdate("MultiPV", numTopMoves)

        this.Go()
        let lines = ResizeArray()
        let mutable continue' = true

        while continue' do
            let text = this.ReadLine()
            let splittedText = text.Split(" ")
            lines.Add(splittedText)

            if splittedText.[0] = "bestmove" then
                continue' <- false

        let topMoves: IReadOnlyDictionary<string, obj> ResizeArray =
            ResizeArray()

        let multiplier =
            if this.GetFenPosition().Contains "w" then 1 else -1

        let reversedLines = lines |> Seq.rev |> ResizeArray
        let mutable i = -1
        continue' <- true

        while continue' do
            &i += 1
            let currentLine = reversedLines.[i]

            if reversedLines |> Seq.tryItem (i + 1) |> Option.isNone then
                continue' <- false

            if currentLine.[0] = "bestmove" then
                if currentLine.[1] = "(none)" then
                    topMoves.Clear()
                    continue' <- false
            elif
                (("multipv" >-< currentLine)
                 && ("depth" >-< currentLine)
                 && int currentLine.[(currentLine |> Seq.findIndex ((=) "depth")) + 1] = this.Depth)
            then
                let multipvNumber =
                    int (currentLine.[(currentLine |> Seq.findIndex ((=) "multipv")) + 1])

                if multipvNumber <= numTopMoves then
                    let hasCentipawnValue = "cp" >-< currentLine
                    let hasMateValue = "mate" >-< currentLine

                    if hasCentipawnValue = hasMateValue then
                        raise
                        <| StockfishRuntimeException(
                            "Having a centipawn value and mate value should be mutually exclusive."
                        )

                    let move =
                        currentLine.[(currentLine |> Seq.findIndex ((=) "pv")) + 1]

                    let centipawn =
                        if hasCentipawnValue then
                            int (currentLine.[(currentLine |> Seq.findIndex ((=) "cp")) + 1])
                            * multiplier
                            |> Nullable
                        else
                            Nullable()

                    let mate =
                        if hasMateValue then
                            int (currentLine.[(currentLine |> Seq.findIndex ((=) "mate")) + 1])
                            * multiplier
                            |> Nullable
                        else
                            Nullable()

                    topMoves.Insert(
                        0,
                        readOnlyDict<string, obj> [
                            "Move", move
                            "Centipawn", centipawn
                            "Mate", mate
                        ]
                    )
            else
                continue' <- false

        if oldMultipvValue <> _parameters.["MultiPV"] then
            this.SetOption("MultiPV", oldMultipvValue)
            _parameters.AddOrUpdate("MultiPV", oldMultipvValue)

        topMoves

    /// <summary>
    /// Sets current depth of stockfish engine.
    /// </summary>
    /// <param name="depthValue">depthValue: Depth option higher than 1</param>
    member this.SetDepth(?depthValue: int) =
        let depthValue = Option.defaultValue 2 depthValue
        _depth <- depthValue

    member this.GetFenParts(?fen: string) =
        let fen = Option.defaultWith this.GetFenPosition fen

        if not <| this.IsFenValid(fen) then
            None
        else
            let reg = Stockfish.Fen.Validator.Patterns.generalFenRegex
            let m = reg.Match(fen)

            if
                not m.Success
                || m.Groups |> Seq.exists (fun x -> not x.Success)
            then
                None
            else
                Some {|
                    PiecePlacement = m.Groups.["PiecePlacement"].Value
                    RankItem = m.Groups.["RankItem"].Captures |> Seq.map (fun c -> c.Value)
                    SideToMove = m.Groups.["SideToMove"].Value
                    Castling = m.Groups.["Castling"].Value
                    EnPassant = m.Groups.["EnPassant"].Value
                    HalfMoveClock = m.Groups.["HalfMoveClock"].Value
                    FullMoveNumber = m.Groups.["FullMoveNumber"].Value
                |}


    /// <summary>
    /// Returns what is on the specified square.
    /// </summary>
    /// <param name="square">The coordinate of the square in question. E.g., e4.</param>
    /// <returns>Either one of the 12 enum members in the Piece enum, or the None object if the square is empty.</returns>
    member this.GetPieceOnSquare(square: string) : Stockfish.Piece option =
        let fileLetter = Char.ToLowerInvariant(square.[0])

        if
            (square.Length <> 2
             || fileLetter < 'a'
             || fileLetter > 'h'
             || square.[1] < '1'
             || square.[1] > '8')
        then
            raise
            <| StockfishValueException("square argument to the get_what_is_on_square function isn't valid.")

        let rankNum = Char.GetNumericValue(square.[1]) |> int

        let rankVisual =
            let vis = this.GetBoardVisual()

            let splits =
                vis.Split("\n", StringSplitOptions.RemoveEmptyEntries)

            splits.[17 - 2 * rankNum]

        let pieceAsChar =
            rankVisual.[2 + (int (fileLetter) - int ('a')) * 4]

        if Char.IsWhiteSpace pieceAsChar then
            None
        else
            Some <| Stockfish.Piece.parse (pieceAsChar)

    /// <summary>
    /// Returns whether the proposed move will be a direct capture, en passant, or not a capture at all.
    /// </summary>
    /// <param name="moveValue">The proposed move, in the notation that Stockfish uses. E.g., "e2e4", "g1f3", etc.</param>
    /// <returns>DIRECT_CAPTURE if the move will be a direct capture. EN_PASSANT if the move is a capture done with en passant. NO_CAPTURE if the move does not capture anything.</returns>
    member this.WillMoveCapture(moveValue: string) : Stockfish.Capture =
        if this.IsMoveCorrect(moveValue) |> not then
            raise
            <| StockfishValueException("The proposed move is not valid in the current position.")

        let (>-<) = Seq.contains

        let startingSquarePiece = this.GetPieceOnSquare(moveValue.[..1])

        let endingSquarePiece = this.GetPieceOnSquare(moveValue.[2..3])

        let mutable result = Stockfish.Capture.NO_CAPTURE

        if not endingSquarePiece.IsNone then
            if bool.Parse(string _parameters.["UCI_Chess960"]) = false then
                result <- Stockfish.Capture.DIRECT_CAPTURE
            else
                // Check for Chess960 castling:
                let castlingPieces = [
                    [
                        Some Stockfish.Piece.WHITE_KING
                        Some Stockfish.Piece.WHITE_ROOK
                    ]
                    [
                        Some Stockfish.Piece.BLACK_KING
                        Some Stockfish.Piece.BLACK_ROOK
                    ]
                ]

                if
                    [
                        startingSquarePiece
                        endingSquarePiece
                    ]
                    >-< castlingPieces
                then
                    result <- Stockfish.Capture.NO_CAPTURE
                else
                    result <- Stockfish.Capture.DIRECT_CAPTURE
        elif
            moveValue.[2..3] = this.GetFenPosition().Split(" ").[3]
            && startingSquarePiece
               >-< [
                   Some Stockfish.Piece.WHITE_PAWN
                   Some Stockfish.Piece.BLACK_PAWN
               ]
        then
            result <- Stockfish.Capture.EN_PASSANT

        result

    /// <summary>
    /// Returns whether the version of Stockfish being used is a development build.
    /// </summary>
    /// <returns>True if the major version is a date, indicating SF is a development build. E.g., 020122 is the major version of the SF development build released on Jan 2, 2022. Otherwise, False is returned (which means the engine is an official release of SF).</returns>
    member this.IsDevelopmentBuildOfEngine() : bool =
        _stockfishMajorVersion >= 10109
        && _stockfishMajorVersion <= 311299

    /// <summary>
    /// Sends the 'quit' command to the Stockfish engine, getting the process to stop.
    /// </summary>
    member this.Quit() =
        if _stockfish.IsRunning() then
            this.Put("quit")

            let mutable i = 0

            while _stockfish.IsRunning() do
                _stockfish.Refresh()
                &i += 1

                if i > 50 then
                    _stockfish.Kill(true)

    member this.Dispose() = _stockfish.Dispose()

    interface IDisposable with
        member this.Dispose() = this.Dispose()
