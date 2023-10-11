//// Define all the possible error types in the application and a
//// function to report them.

import gleam/erlang/atom
import gleam/option
import lox_gleam/types.{LoxValue, Token}

// See this return type? It is a brazen, shameless lie.
@external(erlang, "erlang", "halt")
fn halt(exit_code: Int) -> LoxResult(a)

@external(erlang, "io", "put_chars")
fn stderr(where: atom.Atom, contents: String) -> Nil

pub type LoxError {
  ErlangError(message: String, line: String)
  ParseError(
    message: String,
    line: String,
    value: LoxValue,
    remaining_tokens: option.Option(List(Token)),
  )
  RuntimeError(message: String, line: String)
  ScanError(message: String, line: String)
  TooManyArgumentsError(message: String, line: String)
}

pub type LoxResult(t) =
  Result(t, LoxError)

pub fn report_error(result: LoxResult(a)) -> LoxResult(a) {
  let exit_code = case result {
    Error(ErlangError(..)) -> 0
    Error(ParseError(..)) -> 65
    Error(RuntimeError(..)) -> 70
    Error(ScanError(..)) -> 65
    Error(TooManyArgumentsError(..)) -> 0
    _ -> 0
  }

  case result {
    Ok(_) -> result
    Error(ParseError(..) as error) -> {
      print_parse_error(error)
      halt(exit_code)
    }
    Error(ScanError(message: message, line: line)) -> {
      stderr(
        atom.create_from_string("standard_error"),
        "[line " <> line <> "] Error: " <> message <> "\n",
      )
      halt(exit_code)
    }
    Error(RuntimeError(message: message, line: line)) -> {
      stderr(atom.create_from_string("standard_error"), message)
      stderr(
        atom.create_from_string("standard_error"),
        "\n[line " <> line <> "]\n",
      )
      halt(exit_code)
    }
  }
}

// Unlike the function above, this one does not cause the program to crash.
pub fn print_error(error: LoxError) -> Nil {
  case error {
    ParseError(..) -> print_parse_error(error)
    _ -> Nil
  }
}

fn print_parse_error(error: LoxError) -> Nil {
  let assert ParseError(message: message, line: line, value: value, ..) = error
  stderr(
    atom.create_from_string("standard_error"),
    "[line " <> line <> "] Error at '" <> types.read_value(value) <> "': " <> message <> "\n",
  )
}
