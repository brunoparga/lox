//// Define all the possible error types in the application and a
//// function to report them.

import gleam/io

pub type LoxError {
  ErlangError(message: String)
  ParseError(message: String)
  RuntimeError(message: String)
  ScanError(message: String)
  TooManyArgumentsError
}

pub type LoxResult(t) =
  Result(t, LoxError)

pub fn report_error(result: LoxResult(a)) -> LoxResult(a) {
  let message = case result {
    Error(ErlangError(message)) ->
      "Erlang error when opening file: " <> message <> "."
    Error(ParseError(message)) -> message
    Error(RuntimeError(message)) -> "Runtime error: " <> message
    Error(ScanError(message)) -> "Scan error on line X: " <> message
    Error(TooManyArgumentsError) ->
      "Too many arguments given. Usage: gleam run -- [script]"
    _ -> ""
  }
  io.println_error(message)

  result
}
