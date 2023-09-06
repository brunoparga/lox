//// Define all the possible error types in the application and a
//// function to report them.

import gleam/erlang/atom

@external(erlang, "erlang", "halt")
// See this return type? It is a brazen, shameless lie.
fn halt(exit_code: Int) -> LoxResult(a)

@external(erlang, "io", "put_chars")
fn stderr(where: atom.Atom, contents: String) -> Nil

pub type LoxError {
  ErlangError(message: String)
  ParseError(message: String)
  RuntimeError(message: String)
  ScanError(message: String)
  TooManyArgumentsError(message: String)
  SpecialError(message: String, line: String)
}

pub type LoxResult(t) =
  Result(t, LoxError)

pub fn report_error(result: LoxResult(a)) -> LoxResult(a) {
  let exit_code = case result {
    Error(ErlangError(..)) -> 0
    Error(ParseError(..)) -> 65
    Error(RuntimeError(..)) -> 70
    Error(SpecialError(..)) -> 70
    Error(ScanError(..)) -> 65
    Error(TooManyArgumentsError(..)) -> 0
    _ -> 0
  }

  case result {
    Ok(_) -> result
    Error(error) -> {
      let _ = stderr(atom.create_from_string("standard_error"), error.message)
      let assert SpecialError(line: line, ..) = error
      let _ = stderr(atom.create_from_string("standard_error"), "\n[line " <> line <>"]\n")
      halt(exit_code)
    }
  }
}
