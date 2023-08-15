//// Define all the possible error types in the application and a function
//// to print them.

import gleam/dynamic
import gleam/int
import gleam/io
import gleam/map
import gleam/string
import lox_gleam/token.{Token}
import lox_gleam/ast_types.{Expr, Stmt}

pub type LoxError {
  ErlangError(message: String)
  NotImplementedError
  ParseError(
    message: String,
    line: Int,
    tokens: List(Token),
    exprs: List(Expr),
    stmts: List(Stmt),
  )
  RuntimeError(message: String, values: List(dynamic.Dynamic))
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
    ParseError(message, line, tokens, exprs, stmts) ->
      "Parse error on line " <> int.to_string(line) <> ": " <> message <> "\nTokens left to parse: " <> string.inspect(
        tokens,
      ) <> "\nExpressions parsed: " <> string.inspect(exprs) <> "\nStatements parsed: " <> string.inspect(
        stmts,
      )
    RuntimeError(message, values) ->
      "Runtime error: " <> message <> string.inspect(values)
    ScanError(message, line) ->
      "Scan error on line " <> int.to_string(line) <> ": " <> message
    TooManyArgumentsError ->
      "Too many arguments given. Usage: gleam run -- [script]"
    _ -> ""
  }
  io.println_error(message)
  // This function is called in the same place as others which return maps.
  // Therefore, this one does too. Taipchenk.
  map.new()
}
