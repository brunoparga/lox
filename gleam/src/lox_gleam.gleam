import gleam/erlang
import gleam/erlang/file
import gleam/io
import gleam/string
import lox_gleam/ast_printer
import lox_gleam/error
import lox_gleam/parser
import lox_gleam/scanner

pub fn main() -> error.LoxResult(String) {
  case erlang.start_arguments() {
    [] -> run_prompt()
    [filename] -> run_file(filename)
    _ -> error.handle_error(error.TooManyArgumentsError)
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
    Error(reason) ->
      error.handle_error(error.ErlangError(message: string.inspect(reason)))
  }
}

pub fn run(source: String) {
  let result =
    source
    |> scanner.scan()
    |> parser.parse
  case result {
    Ok(expr) -> {
      expr
      |> ast_printer.print()
      |> io.debug()
      |> Ok()
    }
    Error(error_type) -> error.handle_error(error_type)
  }
}
