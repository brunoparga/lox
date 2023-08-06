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

pub fn scan_tokens(source) {
  let reversed_tokens = do_scan_tokens(source, [], 1)
  case reversed_tokens {
    Ok(tokens) -> Ok(list.reverse(tokens))
    Error(reason) -> Error(reason)
  }
}

fn do_scan_tokens(source, tokens, line) {
  case source == "" {
    True -> do_add_token(Eof, "", tokens, option.None, line)
    False -> scan_regular_tokens(source, tokens, line)
  }
}

/// Functions calling `add_token` have the responsibility to pass it all that it needs; currently
/// what changes, besides the `TokenType`, is also the text that goes into the Token struct.
fn scan_regular_tokens(source, tokens, line) {
  let assert Ok(char) = string.first(source)
  case char {
    // Easy one-character tokens
    "(" -> add_plain_token(source, LeftParen, char, tokens, line, 1)
    ")" -> add_plain_token(source, RightParen, char, tokens, line, 1)
    "{" -> add_plain_token(source, LeftBrace, char, tokens, line, 1)
    "}" -> add_plain_token(source, RightBrace, char, tokens, line, 1)
    "," -> add_plain_token(source, Comma, char, tokens, line, 1)
    "." -> add_plain_token(source, Dot, char, tokens, line, 1)
    "+" -> add_plain_token(source, Plus, char, tokens, line, 1)
    "-" -> add_plain_token(source, Minus, char, tokens, line, 1)
    ";" -> add_plain_token(source, Semicolon, char, tokens, line, 1)
    "*" -> add_plain_token(source, Star, char, tokens, line, 1)

    // Chars that might have equals after them
    "!" | "=" | "<" | ">" -> maybe_equals(source, char, tokens, line)

    // Slashes, comments and whitespace
    "/" -> maybe_comment(source, tokens, line)
    " " | "\r" | "\t" -> do_scan_tokens(advance_one(source), tokens, line)
    "\n" -> do_scan_tokens(advance_one(source), tokens, line + 1)

    // Literals
    "\"" -> add_string(advance_one(source), tokens, line)

    // We shouldn't hit this case.
    _ -> Error(errors.ScanUnexpectedCharacterError)
  }
}

fn maybe_equals(source, char, tokens, line) {
  case string.first(advance_one(source)) {
    Ok("=") -> yes_equals(source, char, tokens, line)
    Ok(_) -> no_equals(source, char, tokens, line)
    Error(_reason) -> Error(errors.ScanUnexpectedEOFError)
  }
}

fn no_equals(source, char, tokens, line) {
  case char {
    "!" -> add_plain_token(source, Bang, char, tokens, line, 1)
    "=" -> add_plain_token(source, Equal, char, tokens, line, 1)
    "<" -> add_plain_token(source, Less, char, tokens, line, 1)
    ">" -> add_plain_token(source, Greater, char, tokens, line, 1)
  }
}

fn yes_equals(source, char, tokens, line) {
  let text = char <> "="
  case char {
    "!" -> add_plain_token(source, BangEqual, text, tokens, line, 2)
    "=" -> add_plain_token(source, EqualEqual, text, tokens, line, 2)
    "<" -> add_plain_token(source, LessEqual, text, tokens, line, 2)
    ">" -> add_plain_token(source, GreaterEqual, text, tokens, line, 2)
  }
}

fn maybe_comment(source, tokens, line) {
  case string.first(advance_one(source)) {
    Ok("/") -> scan_comment(source, tokens, line + 1)
    Ok(_) -> add_plain_token(source, Slash, "/", tokens, line, 1)
    Error(_reason) -> Error(errors.ScanUnexpectedEOFError)
  }
}

fn scan_comment(source, tokens, line) {
  case string.split_once(source, "\n") {
    Ok(#(_comment, new_source)) -> do_scan_tokens(new_source, tokens, line)
    // This means a comment in the last line of the file
    Error(_reason) -> do_scan_tokens("", tokens, line)
  }
}

fn add_string(source, tokens, line) {
  let result = string.split_once(source, "\"")
  case result {
    Ok(#(literal, new_source)) ->
      do_add_string(new_source, tokens, literal, line)
    Error(_reason) -> Error(errors.ScanUnterminatedStringError)
  }
}

fn do_add_string(source, tokens, literal, line) {
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

fn add_plain_token(source, token_type, text, tokens, line, advance) {
  add_token(source, token_type, text, option.None, tokens, line, advance)
}

fn add_string_token(source, literal, tokens, line) {
  let text = "\"" <> literal <> "\""
  // The string has already been advanced
  let advance = 0
  add_token(
    source,
    LoxString,
    text,
    option.Some(literal),
    tokens,
    line,
    advance,
  )
}

/// This function's job is *ONLY* to obtain a new list of tokens, including the one currently being
/// worked on, as well as a new tail of the source code string, without the characters responsible
/// for that particular token.
fn add_token(source, token_type, text, literal, tokens, line, advance) {
  let assert Ok(new_tokens) =
    do_add_token(token_type, text, tokens, literal, line)
  let new_source = string.drop_left(source, advance)
  do_scan_tokens(new_source, new_tokens, line)
}

/// This function's job is *ONLY* to receive information needed to build a token, plus an existing
/// list of tokens, and combine them.
fn do_add_token(token_type, text, tokens, literal, line) {
  let token =
    Token(token_type: token_type, lexeme: text, literal: literal, line: line)
  Ok([token, ..tokens])
}

fn advance_one(source) {
  advance_n(source, 1)
}

fn advance_n(source, n) {
  string.drop_left(source, n)
}
