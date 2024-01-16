# Stockfish

This is a dotnet library for interfacing with stockfish cli application.

> Ported from a [python stockfish library](https://github.com/zhelyabuzhsky/stockfish/)
> by [zhelyabuzhsky](https://github.com/zhelyabuzhsky)

Please see the `Stockfish.Demo` project for a small example of displaying a chess board and taking in user input to move the pieces.

This library was quickly thrown together a while back, so there are many improvements that could be made to this library.

Feel free to open issues and pull requests.

## Prerequisites
This library requires the **stockfish** cli application to be installed, as this library doesn't include it.

Please download the version of stockfish that suits your platform from stockfish's [website](https://stockfishchess.org/download/).

### NOTE:
You can configure this library to point to the downloaded stockfish binary:
```Fsharp
let run () =
	use stockfish = new Stockfish("./stockfish.exe") // <- pass the stockfish binary file path
	let fen = stockfish.GetFenPosition()
	printfn "%s" fen

run ()
```
