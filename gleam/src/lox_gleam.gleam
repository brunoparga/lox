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
import lox_gleam/error
import lox_gleam/interpreter
import lox_gleam/parser
import lox_gleam/scanner

pub fn main() {
  case erlang.start_arguments() {
    [] -> run_prompt()
    [filename] -> run_file(filename)
    _ -> error.handle_error(error.TooManyArgumentsError)
  }
}

fn run_prompt() {
  io.println("Welcome to the Lox REPL.")
  io.println("Enter Lox code to be evaluated, or hit Ctrl+D to exit.")
  do_run_prompt()
  io.println("\nExiting Lox REPL.")
  []
}

fn do_run_prompt() {
  case erlang.get_line("> ") {
    Ok(line) -> {
      run(string.trim(line))
      do_run_prompt()
    }
    Error(_) -> []
  }
}

fn run_file(filename: String) {
  case file.read(from: filename) {
    Ok(contents) -> run(contents)
    Error(error) ->
      error.handle_error(error.ErlangError(message: string.inspect(error)))
  }
}

pub fn run(source: String) {
  source
  |> scanner.scan()
  |> parser.parse()
  |> interpreter.interpret()
}
