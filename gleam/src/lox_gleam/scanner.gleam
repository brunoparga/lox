import gleam/list
import gleam/option
import gleam/string
import lox_gleam/errors
import lox_gleam/token.{Token}
import lox_gleam/token_type.{
  And, Bang, BangEqual, Class, Comma, Dot, Else, Eof, Equal, EqualEqual, For,
  Fun, Greater, GreaterEqual, Identifier, If, LeftBrace, LeftParen, Less,
  LessEqual, LoxFalse, LoxNil, LoxString, LoxTrue, Minus, Number, Or, Plus,
  Print, Return, RightBrace, RightParen, Semicolon, Slash, Star, Super, This,
  Var, While,
}

pub type ScannerType(a) =
  Result(List(Token(a)), errors.LoxGleamError)

pub fn scan_tokens(source) -> ScannerType(a) {
  let reversed_tokens = do_scan_tokens(source, [], 1)
  case reversed_tokens {
    Ok(tokens) -> Ok(list.reverse(tokens))
    Error(reason) -> Error(reason)
  }
}

fn do_scan_tokens(source, tokens, line) -> ScannerType(a) {
  case source == "\n" {
    True -> do_add_token(Eof, "", tokens, option.None, line)
    False -> scan_regular_tokens(source, tokens, line)
  }
}

fn scan_regular_tokens(source, tokens, line) -> ScannerType(a) {
  let assert Ok(char) = string.first(source)
  let result = case char {
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
    _ -> Error(errors.ScanUnexpectedCharacterError)
  }
  case result {
    Ok(#(new_source, new_tokens)) ->
      do_scan_tokens(new_source, new_tokens, line)
    Error(reason) -> Error(reason)
  }
}

fn add_token(source, token_type, tokens, line) {
  let text = string.slice(from: source, at_index: 0, length: 1)
  let assert Ok(new_tokens) =
    do_add_token(token_type, text, tokens, option.None, line)
  let new_source = string.drop_left(source, 1)
  Ok(#(new_source, new_tokens))
}

fn do_add_token(token_type, text, tokens, literal, line) {
  let token =
    Token(token_type: token_type, lexeme: text, literal: literal, line: line)
  Ok([token, ..tokens])
}
