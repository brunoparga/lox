import gleam/list
import gleam/option
import gleam/string
import lox_gleam/token.{Token}
import lox_gleam/token_type.{
  And, Bang, BangEqual, Class, Comma, Dot, Else, Eof, Equal, EqualEqual, For,
  Fun, Greater, GreaterEqual, Identifier, If, LeftBrace, LeftParen, Less,
  LessEqual, LoxFalse, LoxNil, LoxString, LoxTrue, Minus, Number, Or, Plus,
  Print, Return, RightBrace, RightParen, Semicolon, Slash, Star, Super, This,
  Var, While,
}

pub fn scan_tokens(source) -> List(Token(a)) {
  let source_length = string.length(source)
  let reversed_tokens = do_scan_tokens(source, [], 1, source_length)
  list.reverse(reversed_tokens)
}

fn do_scan_tokens(
  source,
  tokens,
  line,
  source_length,
) -> List(Token(a)) {
  case source == "\n" {
    True -> do_add_token(Eof, "", tokens, option.None, line)
    False ->
      scan_regular_tokens(source, tokens, line, source_length)
  }
}

fn scan_regular_tokens(
  source,
  tokens,
  line,
  source_length,
) -> List(Token(a)) {
  let assert Ok(char) = string.first(source)
  let #(new_source, new_tokens) = case char {
    "(" -> add_token(source, LeftParen, tokens, line)
    ")" -> add_token(source, RightParen, tokens, line)
    "{" -> add_token(source, LeftBrace, tokens, line)
    "}" -> add_token(source, RightBrace, tokens, line)
    "," -> add_token(source, Comma, tokens, line)
    "." -> add_token(source, Dot, tokens, line)
    "+" -> add_token(source, Plus, tokens, line)
    "-" -> add_token(source, Minus, tokens, line)
    ";" -> add_token(source, Semicolon, tokens, line)
    "*" -> add_token(source, Star, tokens, line)
    // We shouldn't hit this case.
    _ -> #("", [])
  }
  do_scan_tokens(
    new_source,
    new_tokens,
    line,
    source_length,
  )
}

fn add_token(source, token_type, tokens, line) {
  let token_length = 1
  let text = string.slice(from: source, at_index: 0, length: token_length)
  let new_tokens = do_add_token(token_type, text, tokens, option.None, line)
  let new_source = string.drop_left(source, token_length)
  #(new_source, new_tokens)
}

fn do_add_token(token_type, text, tokens, literal, line) {
  let token =
    Token(token_type: token_type, lexeme: text, literal: literal, line: line)
  [token, ..tokens]
}
