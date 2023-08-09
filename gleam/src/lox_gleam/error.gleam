//// Define all the possible error types in the application and a function
//// to print them.

import gleam/int
import gleam/io

pub type LoxError {
  ErlangError(message: String)
  NotImplementedError
  ParseError(message: String, line: Int)
  ScanError(message: String, line: Int)
  ScanInvalidNumberError
  ScanUnexpectedEOFError
  TooManyArgumentsError
}

pub type LoxResult(t) =
  Result(t, LoxError)

pub fn handle_error(error_type) {
  let message = case error_type {
    ErlangError(message) -> "Erlang error when opening file: " <> message <> "."
    ParseError(message, line) ->
      "Parse error on line " <> int.to_string(line) <> ": " <> message
    ScanError(message, line) ->
      "Scan error on line " <> int.to_string(line) <> ": " <> message
    TooManyArgumentsError ->
      "Too many arguments given. Usage: gleam run -- [script]"
    _ -> ""
  }
  io.println_error(message)
  Error(error_type)
}
