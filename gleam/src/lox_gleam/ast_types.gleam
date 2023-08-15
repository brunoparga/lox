import gleam/dynamic
import lox_gleam/token_type.{TokenType}

pub type Stmt {
  PrintStmt(expression: Expr)
  ExprStmt(expression: Expr)
  VarStmt(name: String, initializer: Expr)
}

pub type Expr {
  Binary(operator: TokenType, left: Expr, right: Expr, line: Int)
  Grouping(expression: Expr, line: Int)
  Literal(value: dynamic.Dynamic, line: Int)
  Unary(operator: TokenType, right: Expr, line: Int)
  Variable(name: String)
}
