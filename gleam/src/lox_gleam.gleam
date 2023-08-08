import gleam/erlang
import gleam/erlang/file
import gleam/io
import gleam/string
import lox_gleam/ast_printer
import lox_gleam/errors
import lox_gleam/parser
import lox_gleam/scanner

pub fn main() {
  case erlang.start_arguments() {
    [] -> run_prompt()
    [filename] -> run_file(filename)
    _ -> {
      io.println_error("Usage: gleam run -- [script]")
      Error(errors.TooManyArgumentsError)
      Nil
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
      Nil
    }
  }
}

pub fn run(source: String) {
  let run_result = scanner.scan_tokens(source)
  case run_result {
    Ok(tokens) -> {
      tokens
      |> parser.parse()
      |> ast_printer.print()
    }
    Error(reason) -> {
      io.debug(Error(reason))
      Nil
    }
  }
}
