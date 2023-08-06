//// Expose scan_tokens, which takes in a string representing Lox source code (whether from a file
//// or typed into the REPL) and returns, hopefully, a list of tokens for that program.

import gleam/dynamic
import gleam/list
import gleam/regex
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

pub fn scan_tokens(source) {
  let reversed_tokens = do_scan_tokens(source, [], 1)
  case reversed_tokens {
    Ok(tokens) -> Ok(list.reverse(tokens))
    Error(reason) -> Error(reason)
  }
}

fn do_scan_tokens(source, tokens, line) {
  case source == "" {
    True -> end_scan(tokens, line)
    False -> scan_regular_tokens(source, tokens, line)
  }
}

/// Functions calling `add_token` have the responsibility to pass it all that it needs; currently
/// what changes, besides the `TokenType`, is also the text that goes into the Token struct.
fn scan_regular_tokens(source, tokens, line) {
  let assert Ok(char) = string.first(source)
  case char {
    // Easy one-character tokens
    "(" | ")" | "{" | "}" | "," | "." | "+" | "-" | ";" | "*" ->
      add_simple_token(advance_one(source), tokens, line, char)

    // Chars that might have equals after them
    "!" | "=" | "<" | ">" ->
      maybe_equals(advance_one(source), char, tokens, line)

    // Slashes, comments and whitespace
    "/" -> maybe_comment(advance_one(source), tokens, line)
    " " | "\r" | "\t" -> do_scan_tokens(advance_one(source), tokens, line)
    "\n" -> do_scan_tokens(advance_one(source), tokens, line + 1)

    // Literals
    "\"" -> add_string(advance_one(source), tokens, line)

    _ -> {
      case is_digit(char) {
        True -> add_number(source, tokens, line)
        False -> Error(errors.ScanUnexpectedCharacterError)
      }
    }
  }
}

fn add_simple_token(source, tokens, line, text) {
  let token =
    Token(
      token_type: text_to_token_type(text),
      lexeme: text,
      literal: dynamic.from(Nil),
      line: line,
    )
  do_scan_tokens(source, [token, ..tokens], line)
}

fn text_to_token_type(text) {
  case text {
    "(" -> LeftParen
    ")" -> RightParen
    "{" -> LeftBrace
    "}" -> RightBrace
    "," -> Comma
    "." -> Dot
    "+" -> Plus
    "-" -> Minus
    ";" -> Semicolon
    "*" -> Star
    "!" -> Bang
    "=" -> Equal
    "<" -> Less
    ">" -> Greater
    "/" -> Slash
    "!=" -> BangEqual
    "==" -> EqualEqual
    "<=" -> LessEqual
    ">=" -> GreaterEqual
  }
}

fn is_digit(char) {
  let assert Ok(regex) = regex.from_string("[0-9]")
  regex.check(regex, char)
}

fn add_number(_source, _tokens, _line) {
  Error(errors.NotImplementedError)
}

fn maybe_equals(source, char, tokens, line) {
  case string.first(source) {
    Ok("=") -> add_simple_token(advance_one(source), tokens, line, char <> "=")
    Ok(_) -> add_simple_token(source, tokens, line, char)
    Error(_reason) -> Error(errors.ScanUnexpectedEOFError)
  }
}

fn maybe_comment(source, tokens, line) {
  case string.first(source) {
    Ok("/") -> scan_comment(source, tokens, line)
    Ok(_) -> add_simple_token(source, tokens, line, "/")
    Error(_reason) -> Error(errors.ScanUnexpectedEOFError)
  }
}

fn scan_comment(source, tokens, line) {
  case string.split_once(source, "\n") {
    Ok(#(_comment, new_source)) -> do_scan_tokens(new_source, tokens, line + 1)
    // This means a comment in the last line of the file
    Error(_reason) -> do_scan_tokens("", tokens, line)
  }
}

fn add_string(source, tokens, line) {
  let result = string.split_once(source, "\"")
  case result {
    Ok(#(literal, new_source)) ->
      do_add_string(new_source, tokens, line, literal)
    Error(_reason) -> Error(errors.ScanUnterminatedStringError)
  }
}

fn do_add_string(source, tokens, line, literal) {
  case string.contains(literal, "\n") {
    True -> {
      let newlines =
        literal
        |> string.to_graphemes()
        |> list.filter(fn(char) { char == "\n" })
        |> list.length()

      add_string_token(source, literal, tokens, line + newlines)
    }
    False -> add_string_token(source, literal, tokens, line)
  }
}

fn add_string_token(source, literal, tokens, line) {
  let token =
    Token(
      token_type: LoxString,
      lexeme: "\"" <> literal <> "\"",
      literal: dynamic.from(literal),
      line: line,
    )
  do_scan_tokens(source, [token, ..tokens], line)
}

fn end_scan(tokens, line) {
  let token =
    Token(token_type: Eof, lexeme: "", literal: dynamic.from(Nil), line: line)
  Ok([token, ..tokens])
}

fn advance_one(source) {
  string.drop_left(source, 1)
}
