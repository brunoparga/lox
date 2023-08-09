import gleam/dynamic
import gleam/list
import lox_gleam/ast_types.{Binary, Expr, Grouping, Literal, Unary}
import lox_gleam/error
import lox_gleam/token.{Token}
import lox_gleam/token_type.{
  Bang, BangEqual, Eof, EqualEqual, Greater, GreaterEqual, LeftParen, Less,
  LessEqual, LoxFalse, LoxNil, LoxString, LoxTrue, Minus, Number, Plus, Slash,
  Star, TokenType,
}

pub fn parse(
  scan_result: Result(List(Token), error.LoxGleamError),
) -> Result(Expr, error.LoxGleamError) {
  case scan_result {
    Ok(tokens) -> {
      let #(expr, _consumed) = expression(tokens)
      Ok(expr)
    }
    Error(reason) -> Error(reason)
  }
}

fn expression(tokens) -> #(Expr, List(Token)) {
  equality(tokens)
}

fn equality(tokens) {
  tokens
  |> comparison()
  |> equality_inner()
}

fn equality_inner(expr_and_tokens) {
  let #(expr, tokens) = expr_and_tokens
  let [first_token, ..tokens1] = tokens
  case match(first_token, [BangEqual, EqualEqual]) {
    True -> {
      let #(right, tokens2) = comparison(tokens1)
      let Token(line: line, token_type: token_type, ..) = first_token
      let term =
        Binary(operator: token_type, left: expr, right: right, line: line)
      equality_inner(#(term, tokens2))
    }
    False -> #(expr, tokens)
  }
}

fn comparison(tokens) {
  tokens
  |> term()
  |> comparison_inner()
}

fn comparison_inner(expr_and_tokens) {
  let #(expr, tokens) = expr_and_tokens
  let [first_token, ..tokens1] = tokens
  case match(first_token, [Greater, GreaterEqual, Less, LessEqual]) {
    True -> {
      let #(right, tokens2) = term(tokens1)
      let Token(line: line, token_type: token_type, ..) = first_token
      let term =
        Binary(operator: token_type, left: expr, right: right, line: line)
      comparison_inner(#(term, tokens2))
    }
    False -> #(expr, tokens)
  }
}

fn term(tokens) {
  tokens
  |> factor()
  |> term_inner()
}

fn term_inner(expr_and_tokens) {
  let #(expr, tokens) = expr_and_tokens
  let [first_token, ..tokens1] = tokens
  case match(first_token, [Minus, Plus]) {
    True -> {
      let #(right, tokens2) = factor(tokens1)
      let Token(line: line, token_type: token_type, ..) = first_token
      let term =
        Binary(operator: token_type, left: expr, right: right, line: line)
      term_inner(#(term, tokens2))
    }
    False -> #(expr, tokens)
  }
}

fn factor(tokens) {
  tokens
  |> unary()
  |> factor_inner()
}

fn factor_inner(expr_and_tokens) {
  let #(expr, tokens) = expr_and_tokens
  let [first_token, ..tokens1] = tokens
  case match(first_token, [Slash, Star]) {
    True -> {
      let #(right, tokens2) = unary(tokens1)
      let Token(line: line, token_type: token_type, ..) = first_token
      let factor =
        Binary(operator: token_type, left: expr, right: right, line: line)
      factor_inner(#(factor, tokens2))
    }
    False -> #(expr, tokens)
  }
}

fn unary(tokens) {
  let [first_token, ..tokens1] = tokens
  case match(first_token, [Bang, Minus]) {
    True -> {
      let #(right, tokens2) = unary(tokens1)
      let Token(line: line, token_type: token_type, ..) = first_token
      let unary_expr = Unary(operator: token_type, right: right, line: line)
      #(unary_expr, tokens2)
    }
    False -> primary(tokens)
  }
}

fn primary(tokens: List(Token)) -> #(Expr, List(Token)) {
  let [first_token, ..other_tokens] = tokens
  case first_token.token_type {
    LoxFalse -> #(
      Literal(value: dynamic.from(False), line: first_token.line),
      other_tokens,
    )
    LoxTrue -> #(
      Literal(value: dynamic.from(True), line: first_token.line),
      other_tokens,
    )
    LoxNil -> #(
      Literal(value: dynamic.from(Nil), line: first_token.line),
      other_tokens,
    )
    LoxString | Number -> #(
      Literal(value: dynamic.from(first_token.literal), line: first_token.line),
      other_tokens,
    )
    LeftParen -> {
      let #(inner_expr, [_right_paren, ..tokens2]) = expression(other_tokens)
      // For now we're only concerned with the happy path; there
      // definitely is a right paren here.
      let expr = Grouping(expression: inner_expr, line: first_token.line)
      #(expr, tokens2)
    }
    Eof -> #(
      Literal(value: dynamic.from(Nil), line: first_token.line),
      other_tokens,
    )
    _ -> #(Literal(value: dynamic.from(Nil), line: first_token.line), tokens)
  }
}

fn match(token: Token, types: List(TokenType)) -> Bool {
  list.any(types, fn(type_to_match) { token.token_type == type_to_match })
}
