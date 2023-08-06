import gleam/dynamic.{Dynamic}
import gleam/string
import lox_gleam/token_type.{TokenType}

pub type Token {
  Token(token_type: TokenType, lexeme: String, literal: Dynamic, line: Int)
}

pub fn print_token(token: Token) -> String {
  string.inspect(token)
}
