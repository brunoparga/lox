import gleam/dynamic
import lox_gleam/token.{Token}
import lox_gleam/token_type.{TokenType}

pub type Stmt {
  PrintStmt(expression: Expr)
  ExprStmt(expression: Expr)
  VarStmt(name: Token, initializer: Expr)
}

pub type Expr {
  Assign(name: Token, value: Expr)
  Binary(operator: TokenType, left: Expr, right: Expr, line: Int)
  Grouping(expression: Expr, line: Int)
  Literal(value: dynamic.Dynamic, line: Int)
  Unary(operator: TokenType, right: Expr, line: Int)
  Variable(name: Token)
}
