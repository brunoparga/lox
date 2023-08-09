import gleam/dynamic
import gleam/list
import lox_gleam/ast_types.{Binary, Expr, Grouping, Literal, Unary}
import lox_gleam/error.{LoxResult, ParseError}
import lox_gleam/token.{Token}
import lox_gleam/token_type.{
  Bang, BangEqual, Eof, EqualEqual, Greater, GreaterEqual, LeftParen, Less,
  LessEqual, LoxFalse, LoxNil, LoxString, LoxTrue, Minus, Number, Plus, Slash,
  Star, TokenType,
}

type ExprsAndTokens =
  #(Expr, List(Token))

pub fn parse(scan_result: LoxResult(List(Token))) -> LoxResult(Expr) {
  case scan_result {
    Ok(tokens) -> do_parse(tokens)
    Error(error.ScanError(..) as error) -> Error(error)
    Error(error) -> handle_error(error)
  }
}

fn handle_error(error) {
  case error {
    ParseError(message: message, line: line, exprs: exprs, tokens: tokens) -> {
      case tokens {
        [] -> Error(error)
        _ -> {
          let assert Ok(#(new_expr, other_tokens)) = expression(tokens)
          handle_error(ParseError(
            message: message,
            line: line,
            exprs: [new_expr, ..exprs],
            tokens: other_tokens,
          ))
        }
      }
    }
    _ -> Error(error)
  }
}

fn do_parse(tokens) {
  case expression(tokens) {
    Ok(#(expr, _consumed)) -> Ok(expr)
    Error(error) -> Error(error)
  }
}

fn expression(tokens) -> LoxResult(ExprsAndTokens) {
  equality(tokens)
}

fn equality(tokens) -> LoxResult(ExprsAndTokens) {
  tokens
  |> comparison()
  |> binary_inner([BangEqual, EqualEqual], comparison)
}

fn comparison(tokens) -> LoxResult(ExprsAndTokens) {
  tokens
  |> term()
  |> binary_inner([Greater, GreaterEqual, Less, LessEqual], term)
}

fn term(tokens) -> LoxResult(ExprsAndTokens) {
  tokens
  |> factor()
  |> binary_inner([Minus, Plus], factor)
}

fn factor(tokens) -> LoxResult(ExprsAndTokens) {
  tokens
  |> unary()
  |> binary_inner([Slash, Star], unary)
}

fn unary(tokens) -> LoxResult(ExprsAndTokens) {
  let [first_token, ..tokens1] = tokens
  case match(first_token, [Bang, Minus]) {
    True -> {
      case unary(tokens1) {
        Ok(#(right, tokens2)) -> {
          let Token(line: line, token_type: token_type, ..) = first_token
          let unary_expr = Unary(operator: token_type, right: right, line: line)
          Ok(#(unary_expr, tokens2))
        }
        Error(error) -> Error(error)
      }
    }
    False -> primary(tokens)
  }
}

fn primary(tokens: List(Token)) -> LoxResult(ExprsAndTokens) {
  let [first_token, ..other_tokens] = tokens
  case first_token.token_type {
    LoxFalse ->
      Ok(#(
        Literal(value: dynamic.from(False), line: first_token.line),
        other_tokens,
      ))
    LoxTrue ->
      Ok(#(
        Literal(value: dynamic.from(True), line: first_token.line),
        other_tokens,
      ))
    LoxNil ->
      Ok(#(
        Literal(value: dynamic.from(Nil), line: first_token.line),
        other_tokens,
      ))
    LoxString | Number ->
      Ok(#(
        Literal(
          value: dynamic.from(first_token.literal),
          line: first_token.line,
        ),
        other_tokens,
      ))
    LeftParen -> {
      case expression(other_tokens) {
        Ok(#(inner_expr, [Token(lexeme: ")", ..), ..tokens2])) -> {
          let expr = Grouping(expression: inner_expr, line: first_token.line)
          Ok(#(expr, tokens2))
        }
        Ok(#(inner_expr, [Token(lexeme: not_right_paren, ..), ..tokens2])) -> {
          Error(ParseError(
            message: "unmatched '(', got " <> not_right_paren <> " instead.",
            line: first_token.line,
            tokens: tokens2,
            exprs: [inner_expr],
          ))
        }
        Error(error) -> Error(error)
      }
    }
    Eof ->
      Ok(#(
        Literal(value: dynamic.from(Nil), line: first_token.line),
        other_tokens,
      ))
    _ ->
      Error(ParseError(
        message: "unexpected token.",
        line: first_token.line,
        tokens: other_tokens,
        exprs: [],
      ))
  }
}

fn match(token: Token, types: List(TokenType)) -> Bool {
  list.any(types, fn(type_to_match) { token.token_type == type_to_match })
}

fn binary_inner(types, function) {
  fn(expr_and_tokens) -> LoxResult(ExprsAndTokens) {
    case expr_and_tokens {
      Ok(#(expr, tokens)) -> happy_path(types, function)(expr, tokens)
      Error(error) -> Error(error)
    }
  }
}

fn happy_path(types, function) {
  fn(expr, tokens) {
    let [first_token, ..other_tokens] = tokens
    case match(first_token, types) {
      True -> build_binary(types, function, expr, first_token, other_tokens)
      False -> Ok(#(expr, tokens))
    }
  }
}

fn build_binary(types, function, left, first_token, other_tokens) {
  case function(other_tokens) {
    Ok(#(right, tokens2)) -> {
      let Token(line: line, token_type: token_type, ..) = first_token
      let binary =
        Binary(operator: token_type, left: left, right: right, line: line)
      binary_inner(types, function)(Ok(#(binary, tokens2)))
    }
    Error(error) -> Error(error)
  }
}
