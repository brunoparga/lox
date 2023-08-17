//// Define all the possible error types in the application and a function
//// to print them.

import gleam/dynamic
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
