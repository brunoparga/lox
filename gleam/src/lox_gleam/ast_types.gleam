import gleam/dynamic
import gleam/option
import lox_gleam/token.{Token}
import lox_gleam/token_type.{TokenType}

pub type Stmt {
  Block(statements: List(Stmt))
  ExprStmt(expression: Expr)
  IfStmt(condition: Expr, then_branch: Stmt, else_branch: option.Option(Stmt))
  PrintStmt(expression: Expr)
  VarStmt(name: Token, initializer: Expr)
}

pub type Expr {
  Assign(name: Token, value: Expr)
  Binary(operator: TokenType, left: Expr, right: Expr, line: Int)
  Grouping(expression: Expr, line: Int)
  Literal(value: dynamic.Dynamic, line: Int)
  Logical(operator: TokenType, left: Expr, right: Expr, line: Int)
  Unary(operator: TokenType, right: Expr, line: Int)
  Variable(name: Token)
}
