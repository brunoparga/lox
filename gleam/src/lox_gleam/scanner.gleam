//// Expose scan, which takes in a string representing Lox source
//// code (whether from a file or typed into the REPL) and returns,
//// hopefully, a list of tokens for that program.

import gleam/float
import gleam/int
import gleam/list
import gleam/regex
import gleam/result
import gleam/string
import lox_gleam/error.{LoxResult, ScanError}
import lox_gleam/types.{
  And, Bang, BangEqual, Class, Comma, Dot, Else, Eof, Equal, EqualEqual, For,
  Fun, Greater, GreaterEqual, Identifier, If, LeftBrace, LeftParen, Less,
  LessEqual, LoxNil, LoxNumber, LoxString, Minus, Or, Plus, Print, Return,
  RightBrace, RightParen, Semicolon, Slash, Star, Super, This, Token, TokenFalse,
  TokenNil, TokenNumber, TokenString, TokenType, TrueToken, Var, While,
}

pub fn scan(source: String) -> LoxResult(List(Token)) {
  result.map(do_scan(source, [], "1"), fn(tokens) { list.reverse(tokens) })
}

fn do_scan(
  source: String,
  tokens: List(Token),
  // The only purpose of hauling these line numbers around is for error
  // reporting, which happens in strings. So it's easier to keep them
  // as strings and convert both ways when arithmetic is needed.
  line: String,
) -> LoxResult(List(Token)) {
  case source == "" {
    // Add EOF if we're done
    True -> end_scan(tokens, line)
    False -> scan_tokens(source, tokens, line)
  }
}

fn end_scan(tokens: List(Token), line: String) -> LoxResult(List(Token)) {
  let token = Token(token_type: Eof, value: LoxNil, line: line)
  Ok([token, ..tokens])
}

fn scan_tokens(
  source: String,
  tokens: List(Token),
  line: String,
) -> LoxResult(List(Token)) {
  let assert Ok(#(char, new_source)) = string.pop_grapheme(source)
  case char {
    // Easy one-character tokens
    "(" | ")" | "{" | "}" | "," | "." | "+" | "-" | ";" | "*" ->
      add_simple_token(new_source, char, tokens, line)

    // Chars that might have equals after them
    "!" | "=" | "<" | ">" -> maybe_equals(new_source, char, tokens, line)

    // Slashes, comments and whitespace
    "/" -> maybe_comment(new_source, tokens, line)
    " " | "\r" | "\t" -> do_scan(new_source, tokens, line)
    "\n" -> do_scan(new_source, tokens, increment_line(line))

    // Strings
    "\"" -> add_string(new_source, tokens, line)

    _ -> {
      case is_digit(char), is_alpha(char) {
        True, False -> add_number(source, tokens, line)
        False, True -> add_text_based(source, tokens, line)
        False, False ->
          Error(ScanError(
            "unexpected character on line " <> line <> ".",
            line: line,
          ))
      }
    }
  }
}

fn add_simple_token(
  source: String,
  text: String,
  tokens: List(Token),
  line: String,
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
    "false" -> TokenFalse
    "for" -> For
    "fun" -> Fun
    "if" -> If
    "nil" -> TokenNil
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
  line: String,
) -> LoxResult(List(Token)) {
  source
  |> string.pop_grapheme()
  |> result.replace_error(ScanError(
    message: "unexpected end of file (line " <> line <> ").",
    line: line,
  ))
  |> result.then(fn(result) {
    case result {
      #("=", new_source) ->
        add_simple_token(new_source, char <> "=", tokens, line)
      _ -> add_simple_token(source, char, tokens, line)
    }
  })
}

fn maybe_comment(
  source: String,
  tokens: List(Token),
  line: String,
) -> LoxResult(List(Token)) {
  source
  |> string.first()
  |> result.replace_error(ScanError(
    message: "unexpected end of file (line " <> line <> ").",
    line: line,
  ))
  |> result.then(fn(result) {
    case result {
      "/" -> scan_comment(source, tokens, line)
      _ -> add_simple_token(source, "/", tokens, line)
    }
  })
}

fn scan_comment(
  source: String,
  tokens: List(Token),
  line: String,
) -> LoxResult(List(Token)) {
  case string.split_once(source, "\n") {
    Ok(#(_comment, new_source)) ->
      do_scan(new_source, tokens, increment_line(line))
    // This means a comment in the last line of the file
    Error(_reason) -> do_scan("", tokens, line)
  }
}

fn add_string(
  source: String,
  tokens: List(Token),
  line: String,
) -> LoxResult(List(Token)) {
  source
  |> string.split_once("\"")
  |> result.replace_error(ScanError(
    message: "unterminated string on line " <> line <> ".",
    line: line,
  ))
  |> result.then(fn(result) {
    let #(literal, new_source) = result
    do_add_string(new_source, tokens, line, literal)
  })
}

fn do_add_string(
  source: String,
  tokens: List(Token),
  line: String,
  text: String,
) -> LoxResult(List(Token)) {
  let newlines = case string.contains(text, "\n") {
    True -> count_newlines(text)
    False -> 0
  }
  let token = Token(token_type: TokenString, value: LoxString(text), line: line)
  do_scan(source, [token, ..tokens], increase_line(line, newlines))
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
  line: String,
) -> LoxResult(List(Token)) {
  number_text("", source, line, False)
  |> result.then(fn(result) {
    let #(text, new_source) = result
    let assert Ok(value) = float.parse(text)
    let token =
      Token(token_type: TokenNumber, value: LoxNumber(value), line: line)
    do_scan(new_source, [token, ..tokens], line)
  })
}

fn number_text(
  current: String,
  source: String,
  line: String,
  decimal_found: Bool,
) -> LoxResult(#(String, String)) {
  let result = string.pop_grapheme(source)
  case result, decimal_found {
    // Trying to have a number with two decimal points
    Ok(#(".", _)), True ->
      Error(ScanError(
        "on line " <> line <> "; a number can only have one decimal point.",line: line
      ))
    // Process the decimal and the first character after it
    Ok(#(".", new_source)), False -> handle_decimal(current, new_source, line)
    // See what the new character is - recurse or return
    Ok(#(char, _)), _ ->
      handle_digit(current, char, source, line, decimal_found)
    // A float at the end of the source
    Error(_), True -> Ok(#(current, source))
    // An integer at the end of the source
    Error(_), False -> Ok(#(current <> ".0", source))
  }
}

fn handle_decimal(
  current: String,
  source: String,
  line: String,
) -> LoxResult(#(String, String)) {
  let assert Ok(#(char, new_source)) = string.pop_grapheme(source)
  // What follows a decimal point must be a digit
  case is_digit(char) {
    // If it is, why not just consume it. We also set the flag
    // showing we're past the decimal.
    True -> {
      let new_current = current <> "." <> char
      number_text(new_current, new_source, line, True)
    }
    False ->
      Error(ScanError(
        "on line " <> line <> "; a decimal point must be followed by a digit.",line: line
      ))
  }
}

fn handle_digit(
  current: String,
  char: String,
  source: String,
  line: String,
  decimal_found: Bool,
) -> LoxResult(#(String, String)) {
  case is_digit(char), decimal_found {
    // If the char is a digit, we append it to the number and recurse.
    True, _ ->
      number_text(
        current <> char,
        string.drop_left(source, 1),
        line,
        decimal_found,
      )
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
  line: String,
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
  line: String,
) -> LoxResult(#(String, String)) {
  source
  |> string.pop_grapheme()
  |> result.replace_error(ScanError(
    message: "unexpected end of file (line " <> line <> ").",
    line: line,
  ))
  |> result.then(fn(result) {
    let #(char, new_source) = result
    case is_alphanumeric(char), new_source {
      // We're done scanning the lexeme
      False, _ -> Ok(#(current, source))
      // We're at the end of the source code
      True, "" -> Ok(#(current <> char, ""))
      // Not done yet, so we recurse
      True, _ -> identifier_text(current <> char, new_source, line)
    }
  })
}

fn increment_line(line: String) -> String {
  increase_line(line, 1)
}

fn increase_line(line: String, amount: Int) -> String {
  line
  |> int.parse()
  // Parses as a big negative number if parsing fails, so any
  // use of the number is obviously wrong
  |> result.unwrap(-1_000_000_000)
  |> int.add(amount)
  |> int.to_string()
}
