import gleam/erlang
import gleam/erlang/file
import gleam/io
import gleam/list
import gleam/string
import lox_gleam/scanner
import lox_gleam/errors

pub fn main() {
  case erlang.start_arguments() {
    [] -> run_prompt()
    [filename] -> run_file(filename)
    _ -> {
      io.println_error("Usage: gleam run -- [script]")
      Error(errors.TooManyArgumentsError)
    }
  }
}

pub fn run_prompt() {
  let assert Ok(line) = erlang.get_line("> ")
  let _ = run(string.trim(line))
  run_prompt()
}

pub fn run_file(filename: String) {
  case file.read(from: filename) {
    Ok(contents) -> run(contents)
    Error(reason) -> {
      reason
      |> string.inspect()
      |> io.println_error()

      Error(errors.ErlangError)
    }
  }
}

pub fn run(source: String) {
  let run_result = scanner.scan_tokens(source)
  case run_result {
    Ok(tokens) -> list.each(tokens, io.debug(_))
    Error(reason) -> {
      io.debug(Error(reason))
      Nil
    }
  }
  run_result
}
