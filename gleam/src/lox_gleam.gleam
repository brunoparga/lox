//// Lox is the simple programming language for which a Java and a C
//// interpreters are demonstrated in the book Crafting Interpreters, by
//// Robert Nystrom (https://craftinginterpreters.com).
////
//// This package aims to be the first complete, published version of the
//// code from the book ported to Gleam, the statically typed language
//// running on Erlang's VM.

import gleam/erlang
import gleam/erlang/file
import gleam/io
import gleam/string
import lox_gleam/ast_printer
import lox_gleam/error
import lox_gleam/interpreter
import lox_gleam/parser
import lox_gleam/scanner

pub fn main() -> String {
  case erlang.start_arguments() {
    [] -> run_prompt()
    [filename] -> run_file(filename)
    _ -> error.handle_error(error.TooManyArgumentsError)
  }
}

fn run_prompt() {
  io.println("Welcome to the Lox REPL.")
  io.print("Enter Lox code to be evaluated, or hit Ctrl+C twice to exit.\n> ")
  do_run_prompt()
  io.debug("Leaving Lox REPL.")
}

fn do_run_prompt() {
  let assert Ok(line) = erlang.get_line("")
  let _ = run(string.trim(line))
  do_run_prompt()
}

fn run_file(filename: String) {
  case file.read(from: filename) {
    Ok(contents) -> run(contents)
    Error(error) ->
      error.handle_error(error.ErlangError(message: string.inspect(error)))
  }
}

pub fn run(source: String) {
  let result =
    source
    |> scanner.scan()
    |> parser.parse()
  case result {
    Ok(expr) -> {
      expr
      |> ast_printer.print()

      let value = expr
      |> interpreter.interpret()

      io.print(value <> "\n> ")

      value
    }
    Error(error_type) -> error.handle_error(error_type)
  }
}
