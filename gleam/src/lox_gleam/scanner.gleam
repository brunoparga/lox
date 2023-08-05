import gleam/list
import gleam/option
import gleam/string
import lox_gleam/token.{Token}
import lox_gleam/token_type.{Eof}

pub fn scan_tokens(source) -> List(Token(a)) {
  let length = string.length(source)
  let reversed_tokens = do_scan_tokens(source, [], 0, 0, 1, length)
  list.reverse(reversed_tokens)
}

fn do_scan_tokens(
  source,
  tokens,
  start,
  current,
  line,
  length,
) -> List(Token(a)) {
  case current >= length {
    True -> add_eof(tokens, line)
    False -> scan_regular_tokens(source, tokens, start, current, line)
  }
}

fn add_eof(tokens, line) -> List(Token(a)) {
  let eof_token =
    Token(token_type: Eof, lexeme: "", literal: option.None, line: line)
  [eof_token, ..tokens]
}

fn scan_regular_tokens(
  _source,
  tokens,
  _start,
  _current,
  _line,
) -> List(Token(a)) {
  tokens
}
