import gleam/option

pub type Stmt {
  Block(statements: List(Stmt))
  ExprStmt(expression: Expr)
  FunDecl(name: Token, params: List(Token), body: List(Stmt))
  IfStmt(condition: Expr, then_branch: Stmt, else_branch: option.Option(Stmt))
  PrintStmt(expression: Expr)
  ReturnStmt(keyword: Token, value: Expr)
  VarDecl(name: Token, initializer: Expr)
  WhileStmt(condition: Expr, body: Stmt)
}

pub type Expr {
  Assign(name: Token, value: Expr)
  Binary(operator: TokenType, left: Expr, right: Expr, line: Int)
  Call(callee: Expr, paren: Token, arguments: List(Expr))
  Grouping(expression: Expr, line: Int)
  Literal(value: LoxValue, line: Int)
  Logical(operator: TokenType, left: Expr, right: Expr, line: Int)
  Unary(operator: TokenType, right: Expr, line: Int)
  Variable(name: Token)
}

pub type Token {
  Token(token_type: TokenType, value: LoxValue, line: Int)
}

pub type LoxValue {
  LoxBool(Bool)
  LoxFunction(arity: Int, declaration: Stmt, to_string: String)
  LoxNil
  LoxNumber(Float)
  LoxString(String)
  NativeFunction(arity: Int, name: String, to_string: String)
  ReturnValue
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
  StringToken
  NumberToken

  // Keywords
  And
  Class
  Else
  FalseToken
  For
  Fun
  If
  NilToken
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
