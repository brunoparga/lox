import gleam/erlang
import gleam/erlang/file
import gleam/io
import gleam/string
import lox_gleam/scanner
import lox_gleam/token

type MainType(a) =
  Result(List(token.Token(a)), Nil)

pub fn main() {
  case erlang.start_arguments() {
    [] -> run_prompt()
    [filename] -> run_file(filename)
    _ -> {
      io.println_error("Usage: gleam run -- [script]")
      Error(Nil)
    }
  }
}

pub fn run_prompt() -> MainType(a) {
  let assert Ok(line) = erlang.get_line("> ")
  let _ = run(string.trim(line))
  run_prompt()
}

pub fn run_file(filename: String) -> MainType(a) {
  case file.read(from: filename) {
    Ok(contents) -> run(contents)
    Error(reason) -> {
      reason
      |> string.inspect()
      |> io.println_error()

      Error(Nil)
    }
  }
}

pub fn run(source: String) -> MainType(a) {
  source
  |> scanner.scan_tokens()
  |> io.debug()
  |> Ok()
}
