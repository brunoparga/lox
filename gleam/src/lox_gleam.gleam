//// Lox is the simple programming language for which a Java and a C
//// interpreters are demonstrated in the book Crafting Interpreters, by
//// Robert Nystrom (https://craftinginterpreters.com).
////
//// This package aims to be the first complete, published version of the
//// code from the book ported to Gleam, the statically typed language
//// running on Erlang's VM.

import argv
import gleam/erlang
import gleam/io
import gleam/option
import gleam/string
import lox_gleam/environment
import lox_gleam/error
import lox_gleam/interpreter
import lox_gleam/parser
import lox_gleam/scanner
import lox_gleam/types
import simplifile

pub fn main() -> error.LoxResult(types.Environment) {
  case argv.load().arguments {
    [] -> run_prompt()
    [filename] -> run_file(filename)
    _ ->
      Error(error.TooManyArgumentsError(
        "Too many arguments given. Usage: gleam run -- [script]",
        "0",
      ))
  }
}

fn run_prompt() -> error.LoxResult(types.Environment) {
  io.println("Welcome to the Lox REPL.")
  io.println("Enter Lox code to be evaluated, or hit Ctrl+D to exit.")
  let environment = do_run_prompt(environment.create(option.None))
  io.println("\nExiting Lox REPL.")
  Ok(environment)
}

fn do_run_prompt(environment: types.Environment) -> types.Environment {
  case erlang.get_line("> ") {
    Ok(line) -> {
      case run(string.trim(line), environment) {
        Ok(new_environment) -> do_run_prompt(new_environment)
        // The REPL session continues if the user messes up
        Error(error) -> {
          io.println_error(error.message)
          do_run_prompt(environment)
        }
      }
    }
    Error(_) -> environment.create(option.None)
  }
}

fn run_file(filename: String) -> error.LoxResult(types.Environment) {
  case simplifile.read(from: filename) {
    Ok(contents) -> run(contents, environment.create(option.None))
    Error(error) ->
      Error(error.ErlangError(message: string.inspect(error), line: "0"))
  }
}

fn run(
  source: String,
  environment: types.Environment,
) -> error.LoxResult(types.Environment) {
  source
  |> scanner.scan()
  |> parser.parse()
  |> interpreter.interpret(environment)
  |> error.report_error()
}

pub fn run_test(filename: String) -> error.LoxResult(types.Environment) {
  let empty = environment.create(option.None)
  let assert Ok(source) =
    simplifile.read("../../craftinginterpreters/test/" <> filename)
  run(source, empty)
}
