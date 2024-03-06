//// This module collects all the types in the application.

import gleam/float
import gleam/dict
import gleam/string
import gleam/option

pub type Table =
  dict.Dict(String, LoxValue)

pub type Environment {
  Global(table: Table)
  Local(table: Table, parent: Environment)
}

pub type Stmt {
  Block(line: String, statements: List(Stmt))
  ExprStmt(line: String, expression: Expr)
  FunDecl(
    line: String,
    name: LoxValue,
    params: List(LoxValue),
    body: List(Stmt),
  )
  IfStmt(
    line: String,
    condition: Expr,
    then_branch: Stmt,
    else_branch: option.Option(Stmt),
  )
  PrintStmt(line: String, expression: Expr)
  ReturnStmt(line: String, value: Expr)
  VarDecl(line: String, name: LoxValue, initializer: Expr)
  WhileStmt(line: String, condition: Expr, body: Stmt)
}

pub type Expr {
  Assign(line: String, name: LoxValue, value: Expr)
  Binary(line: String, operator: TokenType, left: Expr, right: Expr)
  Call(line: String, callee: Expr, arguments: List(Expr))
  Grouping(line: String, expression: Expr)
  Literal(line: String, value: LoxValue)
  Logical(line: String, operator: TokenType, left: Expr, right: Expr)
  Unary(line: String, operator: TokenType, right: Expr)
  Variable(line: String, name: LoxValue)
}

pub type LoxValue {
  LoxBool(Bool)
  LoxFunction(
    arity: Int,
    declaration: Stmt,
    closure: Environment,
    to_string: String,
  )
  LoxNil
  LoxNumber(Float)
  LoxString(String)
  NativeFunction(arity: Int, name: String, to_string: String)
  ReturnValue
}

pub fn read_value(value: LoxValue) -> String {
  case value {
    LoxNumber(number) ->
      number
      |> float.truncate()
      |> string.inspect()
    LoxString(name) -> name
    ReturnValue -> "ReturnValue"
    LoxNil -> "LoxNil"
    _ -> "FFFUUUUUnreachable"
  }
}

pub type Token {
  Token(line: String, token_type: TokenType, value: LoxValue)
}

pub type TokenType {
  // Single-character tokens
  LeftParen
  RightParen
  LeftBrace
  RightBrace
  Comma
  Dot
  Semicolon
  Plus
  Minus
  Star
  Slash

  // One or two character tokens
  Bang
  BangEqual
  Equal
  EqualEqual
  Greater
  GreaterEqual
  Less
  LessEqual

  // Literals
  Identifier
  TokenString
  TokenNumber

  // Keywords
  And
  Class
  Else
  TokenFalse
  For
  Fun
  If
  TokenNil
  Or
  Print
  Return
  Super
  This
  TrueToken
  Var
  While

  Eof
}
