//// This module collects all the types in the application.

import gleam/map
import gleam/option

pub type Table =
  map.Map(LoxValue, LoxValue)

pub type Environment {
  Global(table: Table)
  Local(parent: Environment, table: Table)
}

pub type Stmt {
  Block(statements: List(Stmt), line: String)
  ExprStmt(expression: Expr, line: String)
  FunDecl(
    name: LoxValue,
    params: List(LoxValue),
    body: List(Stmt),
    line: String,
  )
  IfStmt(
    condition: Expr,
    then_branch: Stmt,
    else_branch: option.Option(Stmt),
    line: String,
  )
  PrintStmt(expression: Expr, line: String)
  ReturnStmt(value: Expr, line: String)
  VarDecl(name: LoxValue, initializer: Expr, line: String)
  WhileStmt(condition: Expr, body: Stmt, line: String)
}

pub fn stmt_line(stmt: Stmt) -> String {
  case stmt {
    Block(line: line, ..) -> line
    ExprStmt(line: line, ..) -> line
    FunDecl(line: line, ..) -> line
    IfStmt(line: line, ..) -> line
    PrintStmt(line: line, ..) -> line
    ReturnStmt(line: line, ..) -> line
    VarDecl(line: line, ..) -> line
    WhileStmt(line: line, ..) -> line
  }
}

pub type Expr {
  Assign(name: LoxValue, value: Expr, line: String)
  Binary(operator: TokenType, left: Expr, right: Expr, line: String)
  Call(callee: Expr, arguments: List(Expr), line: String)
  Grouping(expression: Expr, line: String)
  Literal(value: LoxValue, line: String)
  Logical(operator: TokenType, left: Expr, right: Expr, line: String)
  Unary(operator: TokenType, right: Expr, line: String)
  Variable(name: LoxValue, line: String)
}

pub fn expr_line(expr: Expr) -> String {
  case expr {
    Assign(line: line, ..) -> line
    Binary(line: line, ..) -> line
    Call(line: line, ..) -> line
    Grouping(line: line, ..) -> line
    Literal(line: line, ..) -> line
    Logical(line: line, ..) -> line
    Unary(line: line, ..) -> line
    Variable(line: line, ..) -> line
  }
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

pub type Token {
  Token(token_type: TokenType, value: LoxValue, line: String)
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
