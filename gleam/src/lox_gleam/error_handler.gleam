import gleam/int
import gleam/io
import gleam/option
import gleam/string
import lox_gleam/environment
import lox_gleam/error

pub fn handle_error(error_type) {
  let message = case error_type {
    error.ErlangError(message) ->
      "Erlang error when opening file: " <> message <> "."
    error.ParseError(message) -> message
    error.RuntimeError(message, values) ->
      "Runtime error: " <> message <> string.inspect(values)
    error.ScanError(message, line) ->
      "Scan error on line " <> int.to_string(line) <> ": " <> message
    error.TooManyArgumentsError ->
      "Too many arguments given. Usage: gleam run -- [script]"
    _ -> ""
  }
  io.println_error(message)
  // This function is called in the same place as others which return maps.
  // Therefore, this one does too. Taipchenk.
  environment.create(option.None)
}
