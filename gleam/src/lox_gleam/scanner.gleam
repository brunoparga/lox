//// Expose scan_tokens, which takes in a string representing Lox source code (whether from a file
//// or typed into the REPL) and returns, hopefully, a list of tokens for that program.

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

/// Functions calling `add_token` have the responsibility to pass it all that it needs; currently
/// what changes, besides the `TokenType`, is also the text that goes into the Token struct.
fn scan_regular_tokens(source, tokens, line) -> ScannerType(a) {
  let assert Ok(char) = string.first(source)
  let result = case char {
    // Easy one-character tokens
    "(" -> add_token(source, LeftParen, char, tokens, line)
    ")" -> add_token(source, RightParen, char, tokens, line)
    "{" -> add_token(source, LeftBrace, char, tokens, line)
    "}" -> add_token(source, RightBrace, char, tokens, line)
    "," -> add_token(source, Comma, char, tokens, line)
    "." -> add_token(source, Dot, char, tokens, line)
    "+" -> add_token(source, Plus, char, tokens, line)
    "-" -> add_token(source, Minus, char, tokens, line)
    ";" -> add_token(source, Semicolon, char, tokens, line)
    "*" -> add_token(source, Star, char, tokens, line)

    // Chars that might have equals after them
    "!" | "=" | "<" | ">" -> maybe_equals(source, char, tokens, line)
    // We shouldn't hit this case.
    _ -> Error(errors.ScanUnexpectedCharacterError)
  }
  case result {
    Ok(#(new_source, new_tokens)) ->
      do_scan_tokens(new_source, new_tokens, line)
    Error(reason) -> Error(reason)
  }
}

fn maybe_equals(source, char, tokens, line) {
  let tmp_source = string.drop_left(source, 1)
  case string.first(tmp_source) {
    Ok("=") -> yes_equals(source, char, tokens, line)
    Ok(_) -> no_equals(source, char, tokens, line)
    Error(_reason) -> Error(errors.ScanUnexpectedCharacterError)
  }
}

fn no_equals(source, char, tokens, line) {
  case char {
    "!" -> add_token(source, Bang, char, tokens, line)
    "=" -> add_token(source, Equal, char, tokens, line)
    "<" -> add_token(source, Less, char, tokens, line)
    ">" -> add_token(source, Greater, char, tokens, line)
  }
}

fn yes_equals(source, char, tokens, line) {
  let text = char <> "="
  case char {
    "!" -> add_token(source, BangEqual, text, tokens, line)
    "=" -> add_token(source, EqualEqual, text, tokens, line)
    "<" -> add_token(source, LessEqual, text, tokens, line)
    ">" -> add_token(source, GreaterEqual, text, tokens, line)
  }
}

/// This function's job is *ONLY* to obtain a new list of tokens, including the one currently being
/// worked on, as well as a new tail of the source code string, without the characters responsible
/// for that particular token.
fn add_token(source, token_type, text, tokens, line) {
  let assert Ok(new_tokens) =
    do_add_token(token_type, text, tokens, option.None, line)
  let new_source = string.drop_left(source, string.length(text))
  Ok(#(new_source, new_tokens))
}

/// This function's job is *ONLY* to receive information needed to build a token, plus an existing
/// list of tokens, and combine them.
fn do_add_token(token_type, text, tokens, literal, line) {
  let token =
    Token(token_type: token_type, lexeme: text, literal: literal, line: line)
  Ok([token, ..tokens])
}
