import gleam/dynamic
import gleam/option
import lox_gleam/token.{Token}
import lox_gleam/token_type.{TokenType}

pub type Stmt {
  Block(statements: List(Stmt))
  ExprStmt(expression: Expr)
  FunDecl(name: Token, params: List(Token), body: List(Stmt))
  IfStmt(condition: Expr, then_branch: Stmt, else_branch: option.Option(Stmt))
  PrintStmt(expression: Expr)
  VarDecl(name: Token, initializer: Expr)
  WhileStmt(condition: Expr, body: Stmt)
}

pub type Expr {
  Assign(name: Token, value: Expr)
  Binary(operator: TokenType, left: Expr, right: Expr, line: Int)
  Call(callee: Expr, paren: Token, arguments: List(Expr))
  Grouping(expression: Expr, line: Int)
  Literal(value: dynamic.Dynamic, line: Int)
  Logical(operator: TokenType, left: Expr, right: Expr, line: Int)
  Unary(operator: TokenType, right: Expr, line: Int)
  Variable(name: Token)
}
