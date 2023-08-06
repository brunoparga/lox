//// Expose scan_tokens, which takes in a string representing Lox source
//// code (whether from a file or typed into the REPL) and returns,
//// hopefully, a list of tokens for that program.

import gleam/dynamic
import gleam/float
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
    // Add EOF if we're done
    True -> end_scan(tokens, line)
    False -> scan_regular_tokens(source, tokens, line)
  }
}

fn scan_regular_tokens(source, tokens, line) {
  let assert Ok(#(char, new_source)) = string.pop_grapheme(source)
  case char {
    // Easy one-character tokens
    "(" | ")" | "{" | "}" | "," | "." | "+" | "-" | ";" | "*" ->
      add_simple_token(new_source, tokens, line, char)

    // Chars that might have equals after them
    "!" | "=" | "<" | ">" -> maybe_equals(new_source, char, tokens, line)

    // Slashes, comments and whitespace
    "/" -> maybe_comment(new_source, tokens, line)
    " " | "\r" | "\t" -> do_scan_tokens(new_source, tokens, line)
    "\n" -> do_scan_tokens(new_source, tokens, line + 1)

    // Strings
    "\"" -> add_string(new_source, tokens, line)

    _ -> {
      case is_digit(char), is_alpha(char) {
        True, False -> add_number(source, tokens, line)
        False, True -> add_identifier(source, tokens, line)
        False, False -> Error(errors.ScanUnexpectedCharacterError)
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
    "and" -> And
    "class" -> Class
    "else" -> Else
    "false" -> LoxFalse
    "for" -> For
    "fun" -> Fun
    "if" -> If
    "nil" -> LoxNil
    "or" -> Or
    "print" -> Print
    "return" -> Return
    "super" -> Super
    "this" -> This
    "true" -> LoxTrue
    "var" -> Var
    "while" -> While
    _ -> Identifier
  }
}

fn is_alpha(char) {
  let assert Ok(regex) = regex.from_string("[a-zA-Z_]")
  regex.check(regex, char)
}

fn is_digit(char) {
  let assert Ok(regex) = regex.from_string("[0-9]")
  regex.check(regex, char)
}

fn add_identifier(source, tokens, line) {
  let result = identifier_text("", source)
  case result {
    Ok(#(text, new_source)) -> {
      let token =
        Token(
          token_type: text_to_token_type(text),
          lexeme: text,
          literal: dynamic.from(Nil),
          line: line,
        )
      do_scan_tokens(new_source, [token, ..tokens], line)
    }
    Error(reason) -> Error(reason)
  }
}

fn identifier_text(current, source) {
  // We ensure this is never called with an empty string as source.
  let assert Ok(#(char, new_source)) = string.pop_grapheme(source)
  case is_alphanumeric(char), new_source {
    // We're done scanning the lexeme
    False, _ -> Ok(#(current, source))
    // We're at the end of the source code
    True, "" -> Ok(#(current <> char, ""))
    // Not done yet, so we recurse
    True, _ -> identifier_text(current <> char, new_source)
  }
}

fn is_alphanumeric(char) {
  is_alpha(char) || is_digit(char)
}

fn add_number(source, tokens, line) {
  let result = number_text("", source, False)
  case result {
    Ok(#(text, new_source)) -> {
      let assert Ok(value) = float.parse(text)
      let token =
        Token(
          token_type: Number,
          lexeme: text,
          literal: dynamic.from(value),
          line: line,
        )
      do_scan_tokens(new_source, [token, ..tokens], line)
    }
    Error(reason) -> Error(reason)
  }
}

fn number_text(current, source, decimal_found) {
  // We ensure this is never called with an empty string as source.
  let assert Ok(#(char, new_source)) = string.pop_grapheme(source)
  case char, new_source, decimal_found {
    // Can't end the file with the decimal dot of a number.
    ".", "", _ -> Error(errors.ScanInvalidNumberError)
    // But it's okay if it's a digit.
    _, "", True -> Ok(#(current <> char, ""))
    _, "", False -> Ok(#(current <> char <> ".0", ""))
    // Now we handle a decimal point. It must be the only one
    ".", _, True -> Error(errors.ScanInvalidNumberError)
    ".", _, False -> handle_decimal(current, new_source)
    // Now for the most general case: we're not done scanning the file,
    // the character is not a dot, and the decimal flag is irrelevant.
    _, _, _ -> handle_digit(current, char, source, decimal_found)
  }
}

fn handle_decimal(current, source) {
  let assert Ok(#(char, new_source)) = string.pop_grapheme(source)
  // What follows a decimal point must be a digit
  case is_digit(char) {
    // If it is, we just consume it while we're at it.
    // We also set the flag showing we've passed the decimal.
    True -> {
      let new_current = current <> "." <> char
      number_text(new_current, new_source, True)
    }
    False -> Error(errors.ScanInvalidNumberError)
  }
}

fn handle_digit(current, char, source, decimal_found) {
  case is_digit(char), decimal_found {
    // If the char is a digit, we append it to the number and recurse.
    True, _ ->
      number_text(current <> char, string.drop_left(source, 1), decimal_found)
    // Otherwise, we're done!
    False, True -> Ok(#(current, source))
    False, False -> Ok(#(current <> ".0", source))
  }
}

fn maybe_equals(source, char, tokens, line) {
  let result = string.pop_grapheme(source)
  case result {
    Ok(#("=", new_source)) ->
      add_simple_token(new_source, tokens, line, char <> "=")
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

fn do_add_string(source, tokens, line, text) {
  let newlines = case string.contains(text, "\n") {
    True -> count_newlines(text)
    False -> 0
  }
  add_string_token(source, text, tokens, line + newlines)
}

fn count_newlines(text) {
  text
  |> string.to_graphemes()
  |> list.filter(fn(char) { char == "\n" })
  |> list.length()
}

fn add_string_token(source, literal, tokens, line) {
  let token =
    Token(
      token_type: LoxString,
      lexeme: "'" <> literal <> "'",
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
