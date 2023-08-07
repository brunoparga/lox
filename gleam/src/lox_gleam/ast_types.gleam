import gleam/dynamic
import lox_gleam/token.{Token}

pub type Expr {
  Binary(left: Expr, operator: Token, right: Expr)
  Grouping(expression: Expr)
  Literal(value: dynamic.Dynamic)
  Unary(operator: Token, right: Expr)
}
