# Download stockfish cli

1. Navigate to https://stockfishchess.org/download/
2. Download your platform's version of the stockfish (Windows/Mac/Linux/etc.)
3. Place downloaded stockfish cli app, into this repo's `stockfish-cli/` folder

NOTE:
Currently tests in the `Stockfish.Lib.Test.Integration` project expects certain naming of the stockfish cli file:

```FSharp
match (OperatingSystem.IsWindows(), OperatingSystem.IsMacOS(), OperatingSystem.IsLinux()) with
| (true, _, _) -> "win-stockfish16.exe"
| (_, true, _) -> "mac-stockfish16"
| (_, _, true) -> "lin-stockfish16"
| _ -> "stockfish"
```

If you're planning on running tests, please download the stockfish cli app and rename it to match your platform.
