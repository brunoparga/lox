import gleam/int
import gleam/io
import lox_gleam/error

pub fn handle_error(error_type: error.LoxError) -> error.LoxResult(a) {
  let message = case error_type {
    error.ErlangError(message) ->
      "Erlang error when opening file: " <> message <> "."
    error.ParseError(message) -> message
    error.RuntimeError(message) -> "Runtime error: " <> message
    error.ScanError(message, line) ->
      "Scan error on line " <> int.to_string(line) <> ": " <> message
    error.TooManyArgumentsError ->
      "Too many arguments given. Usage: gleam run -- [script]"
    _ -> ""
  }
  io.println_error(message)

  Error(error_type)
}
