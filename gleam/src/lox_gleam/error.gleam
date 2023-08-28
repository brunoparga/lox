//// Define all the possible error types in the application.

pub type LoxError {
  ErlangError(message: String)
  NotImplementedError
  ParseError(message: String)
  RuntimeError(message: String)
  ScanError(message: String, line: Int)
  ScanInvalidNumberError
  ScanUnexpectedEOFError
  TooManyArgumentsError
}

pub type LoxResult(t) =
  Result(t, LoxError)
