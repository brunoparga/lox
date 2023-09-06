//// Define all the possible error types in the application and a
//// function to report them.

import gleam/erlang/atom
import lox_gleam/types.{LoxValue}

// See this return type? It is a brazen, shameless lie.
@external(erlang, "erlang", "halt")
fn halt(exit_code: Int) -> LoxResult(a)

@external(erlang, "io", "put_chars")
fn stderr(where: atom.Atom, contents: String) -> Nil

pub type LoxError {
  ErlangError(message: String, line: String)
  ParseError(message: String, line: String, token: LoxValue)
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
    Error(ParseError(message: message, line: line, token: token)) -> {
      stderr(
        atom.create_from_string("standard_error"),
        "[" <> line <> "] Error at '" <> types.read_value(token) <> "': " <> message,
      )
      halt(exit_code)
    }
    Error(RuntimeError(message: message, line: line)) -> {
      let _ = stderr(atom.create_from_string("standard_error"), message)
      let _ =
        stderr(
          atom.create_from_string("standard_error"),
          "\n[line " <> line <> "]\n",
        )
      halt(exit_code)
    }
  }
}
