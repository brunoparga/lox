//// Define all the possible error types in the application and a function
//// to print them.

import gleam/dynamic

pub type LoxError {
  ErlangError(message: String)
  NotImplementedError
  ParseError(message: String)
  RuntimeError(message: String, values: List(dynamic.Dynamic))
  ScanError(message: String, line: Int)
  ScanInvalidNumberError
  ScanUnexpectedEOFError
  TooManyArgumentsError
}

pub type LoxResult(t) =
  Result(t, LoxError)
