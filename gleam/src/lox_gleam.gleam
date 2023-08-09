import gleam/erlang
import gleam/erlang/file
import gleam/io
import gleam/string
import lox_gleam/ast_printer
import lox_gleam/errors.{LoxGleamError}
import lox_gleam/parser
import lox_gleam/scanner

pub fn main() -> Result(String, LoxGleamError) {
  case erlang.start_arguments() {
    [] -> run_prompt()
    [filename] -> run_file(filename)
    _ -> {
      io.println_error("Usage: gleam run -- [script]")
      Error(errors.TooManyArgumentsError)
    }
  }
}

fn run_prompt() {
  io.println("Welcome to the Lox REPL.")
  io.println("Enter Lox code to be evaluated, or hit Ctrl+C twice to exit")
  do_run_prompt()
  Ok("Leaving Lox REPL.")
}

fn do_run_prompt() {
  let assert Ok(line) = erlang.get_line("> ")
  let _ = run(string.trim(line))
  do_run_prompt()
}

fn run_file(filename: String) {
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
    Ok(tokens) -> {
      tokens
      |> parser.parse()
      |> ast_printer.print()
      |> io.debug()
      |> Ok()
    }
    Error(reason) -> {
      reason
      |> io.debug()
      |> Error()
    }
  }
}
