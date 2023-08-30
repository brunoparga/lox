//// Expose scan, which takes in a string representing Lox source
//// code (whether from a file or typed into the REPL) and returns,
//// hopefully, a list of tokens for that program.

import gleam/float
import gleam/list
import gleam/regex
import gleam/result
import gleam/string
import lox_gleam/error.{LoxResult, ScanError}
import lox_gleam/types.{
  And, Bang, BangEqual, Class, Comma, Dot, Else, Eof, Equal, EqualEqual,
  FalseToken, For, Fun, Greater, GreaterEqual, Identifier, If, LeftBrace,
  LeftParen, Less, LessEqual, LoxNil, LoxNumber, LoxString, Minus, NilToken,
  NumberToken, Or, Plus, Print, Return, RightBrace, RightParen, Semicolon, Slash,
  Star, StringToken, Super, This, Token, TokenType, TrueToken, Var, While,
}

pub fn scan(source: String) -> LoxResult(List(Token)) {
  result.try(do_scan(source, [], 1), fn(tokens) { list.reverse(tokens) })
}

fn do_scan(
  source: String,
  tokens: List(Token),
  line: Int,
) -> LoxResult(List(Token)) {
  case source == "" {
    // Add EOF if we're done
    True -> end_scan(tokens, line)
    False -> scan_tokens(source, tokens, line)
  }
}

fn end_scan(tokens: List(Token), line: Int) -> LoxResult(List(Token)) {
  let token = Token(token_type: Eof, value: LoxNil, line: line)
  Ok([token, ..tokens])
}

fn scan_tokens(
  source: String,
  tokens: List(Token),
  line: Int,
) -> LoxResult(List(Token)) {
  let assert Ok(#(char, new_source)) = string.pop_grapheme(source)
  case char {
    // Easy one-character tokens
    "(" | ")" | "{" | "}" | "," | "." | "+" | "-" | ";" | "*" ->
      add_simple_token(new_source, tokens, line, char)

    // Chars that might have equals after them
    "!" | "=" | "<" | ">" -> maybe_equals(new_source, char, tokens, line)

    // Slashes, comments and whitespace
    "/" -> maybe_comment(new_source, tokens, line)
    " " | "\r" | "\t" -> do_scan(new_source, tokens, line)
    "\n" -> do_scan(new_source, tokens, line + 1)

    // Strings
    "\"" -> add_string(new_source, tokens, line)

    _ -> {
      case is_digit(char), is_alpha(char) {
        True, False -> add_number(source, tokens, line)
        False, True -> add_text_based(source, tokens, line)
        False, False ->
          Error(ScanError(
            "unexpected character on line " <> string.inspect(line) <> ".",
          ))
      }
    }
  }
}

fn add_simple_token(
  source: String,
  tokens: List(Token),
  line: Int,
  text: String,
) -> LoxResult(List(Token)) {
  let token =
    Token(token_type: text_to_token_type(text), value: LoxNil, line: line)
  do_scan(source, [token, ..tokens], line)
}

fn text_to_token_type(text: String) -> TokenType {
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
    "and" -> And
    "class" -> Class
    "else" -> Else
    "false" -> FalseToken
    "for" -> For
    "fun" -> Fun
    "if" -> If
    "nil" -> NilToken
    "or" -> Or
    "print" -> Print
    "return" -> Return
    "super" -> Super
    "this" -> This
    "true" -> TrueToken
    "var" -> Var
    "while" -> While
    _ -> Identifier
  }
}

fn maybe_equals(
  source: String,
  char: String,
  tokens: List(Token),
  line: Int,
) -> LoxResult(List(Token)) {
  source
  |> string.pop_grapheme()
  |> result.then(fn(result) {
    case result {
      #("=", new_source) ->
        add_simple_token(new_source, tokens, line, char <> "=")
      _ -> add_simple_token(source, tokens, line, char)
    }
  })
  |> result.map_error(fn(_) {
    Error(ScanError(
      message: "unexpected end of file (line " <> string.inspect(line) <> ").",
    ))
  })
}

fn maybe_comment(
  source: String,
  tokens: List(Token),
  line: Int,
) -> LoxResult(List(Token)) {
  source
  |> string.first()
  |> result.then(fn(result) {
    case string.first(source) {
      "/" -> scan_comment(source, tokens, line)
      _ -> add_simple_token(source, tokens, line, "/")
    }
  })
  |> result.map_error(fn(_) {
    Error(ScanError(
      message: "unexpected end of file (line " <> string.inspect(line) <> ").",
    ))
  })
}

fn scan_comment(
  source: String,
  tokens: List(Token),
  line: Int,
) -> LoxResult(List(Token)) {
  case string.split_once(source, "\n") {
    Ok(#(_comment, new_source)) -> do_scan(new_source, tokens, line + 1)
    // This means a comment in the last line of the file
    Error(_reason) -> do_scan("", tokens, line)
  }
}

fn add_string(
  source: String,
  tokens: List(Token),
  line: Int,
) -> LoxResult(List(Token)) {
  source
  |> string.split_once("\"")
  |> result.then(fn(result) {
    let #(literal, new_source) = result
    do_add_string(new_source, tokens, line, literal)
  })
  |> result.map_error(fn(_) {
    Error(ScanError(
      message: "unterminated string on line " <> string.inspect(line) <> ".",
    ))
  })
}

fn do_add_string(
  source: String,
  tokens: List(Token),
  line: Int,
  text: String,
) -> LoxResult(List(Token)) {
  let newlines = case string.contains(text, "\n") {
    True -> count_newlines(text)
    False -> 0
  }
  let token = Token(token_type: StringToken, value: LoxString(text), line: line)
  do_scan(source, [token, ..tokens], line + newlines)
}

fn count_newlines(text: String) -> Int {
  text
  |> string.to_graphemes()
  |> list.filter(fn(char) { char == "\n" })
  |> list.length()
}

fn is_digit(char: String) -> Bool {
  let assert Ok(regex) = regex.from_string("[0-9]")
  regex.check(regex, char)
}

fn is_alpha(char: String) -> Bool {
  let assert Ok(regex) = regex.from_string("[a-zA-Z_]")
  regex.check(regex, char)
}

fn add_number(
  source: String,
  tokens: List(Token),
  line: Int,
) -> LoxResult(List(Token)) {
  number_text("", source, False)
  |> result.then(fn(result) {
    let #(text, new_source) = result
    let assert Ok(value) = float.parse(text)
    let token =
      Token(token_type: NumberToken, value: LoxNumber(value), line: line)
    do_scan(new_source, [token, ..tokens], line)
  })
}

fn number_text(
  current: String,
  source: String,
  decimal_found: Bool,
) -> LoxResult(#(String, String)) {
  let result = string.pop_grapheme(source)
  case result, decimal_found {
    // Trying to have a number with two decimal points
    Ok(#(".", _)), True ->
      Error(ScanError(
        "on line " <> string.inspect(line) <> "; a number can only have one decimal point.",
      ))
    // Process the decimal and the first character after it
    Ok(#(".", new_source)), False -> handle_decimal(current, new_source)
    // See what the new character is - recurse or return
    Ok(#(char, _)), _ -> handle_digit(current, char, source, decimal_found)
    // A float at the end of the source
    Error(_), True -> Ok(#(current, source))
    // An integer at the end of the source
    Error(_), False -> Ok(#(current <> ".0", source))
  }
}

fn handle_decimal(
  current: String,
  source: String,
) -> LoxResult(#(String, String)) {
  let assert Ok(#(char, new_source)) = string.pop_grapheme(source)
  // What follows a decimal point must be a digit
  case is_digit(char) {
    // If it is, why not just consume it. We also set the flag
    // showing we're past the decimal.
    True -> {
      let new_current = current <> "." <> char
      number_text(new_current, new_source, True)
    }
    False ->
      Error(ScanError(
        "on line " <> string.inspect(line) <> "; a decimal point must be followed by a digit.",
      ))
  }
}

fn handle_digit(
  current: String,
  char: String,
  source: String,
  decimal_found: Bool,
) -> LoxResult(#(String, String)) {
  case is_digit(char), decimal_found {
    // If the char is a digit, we append it to the number and recurse.
    True, _ ->
      number_text(current <> char, string.drop_left(source, 1), decimal_found)
    // Otherwise, we're done!
    False, True -> Ok(#(current, source))
    False, False -> Ok(#(current <> ".0", source))
  }
}

fn is_alphanumeric(char: String) -> Bool {
  is_alpha(char) || is_digit(char)
}

// Handle identifiers and also tokens like "print" or "return", whose
// text field is alphanumeric
fn add_text_based(
  source: String,
  tokens: List(Token),
  line: Int,
) -> LoxResult(List(Token)) {
  identifier_text("", source, line)
  |> result.then(fn(result) {
    let #(text, new_source) = result
    let token_type = text_to_token_type(text)
    let value = case token_type {
      Identifier -> LoxString(text)
      _ -> LoxNil
    }
    let token = Token(token_type: token_type, value: value, line: line)
    do_scan(new_source, [token, ..tokens], line)
  })
}

fn identifier_text(
  current: String,
  source: String,
  line: Int,
) -> LoxResult(#(String, String)) {
  source
  |> string.pop_grapheme()
  |> result.then(fn(result) {
    let #(char, new_source) = result
    case is_alphanumeric(char), new_source {
      // We're done scanning the lexeme
      False, _ -> #(current, source)
      // We're at the end of the source code
      True, "" -> #(current <> char, "")
      // Not done yet, so we recurse
      True, _ -> identifier_text(current <> char, new_source, line)
    }
  })
  |> result.map_error(fn(_) {
    Error(ScanError(
      message: "unexpected end of file (line " <> string.inspect(line) <> ").",
    ))
  })
}
