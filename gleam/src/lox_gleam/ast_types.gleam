import gleam/dynamic
import lox_gleam/token_type.{TokenType}

pub type Expr {
  Binary(operator: TokenType, left: Expr, right: Expr, line: Int)
  Grouping(expression: Expr, line: Int)
  Literal(value: dynamic.Dynamic, line: Int)
  Unary(operator: TokenType, right: Expr, line: Int)
}
