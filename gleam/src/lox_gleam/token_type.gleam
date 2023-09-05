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
  LoxString
  Number

  // Keywords
  And
  Class
  Else
  LoxFalse
  For
  Fun
  If
  LoxNil
  Or
  Print
  Return
  Super
  This
  LoxTrue
  Var
  While

  Eof
}
