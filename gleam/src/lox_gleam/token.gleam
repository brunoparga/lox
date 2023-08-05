import gleam/option
import gleam/string
import lox_gleam/token_type.{TokenType}

pub type Token(literal_type) {
  Token(
    token_type: TokenType,
    lexeme: String,
    literal: option.Option(literal_type),
    line: Int,
  )
}

pub fn print_token(token: Token(literal_type)) -> String {
  string.inspect(token)
}
