//// Expose `parse`, a function that takes in a list of Lox tokens and
//// returns an abstract syntax tree.

import gleam/dynamic
import gleam/list
import lox_gleam/ast_types.{
  Binary, Expr, ExprStmt, Grouping, Literal, PrintStmt, Stmt, Unary,
}
import lox_gleam/error.{LoxResult, ParseError}
import lox_gleam/token.{Token}
import lox_gleam/token_type.{
  Bang, BangEqual, Eof, EqualEqual, Greater, GreaterEqual, LeftParen, Less,
  LessEqual, LoxFalse, LoxNil, LoxString, LoxTrue, Minus, Number, Plus, Print,
  Slash, Star, TokenType,
}

type ExprsAndTokens =
  #(Expr, List(Token))

type StmtsAndTokens =
  #(List(Stmt), List(Token))

pub fn parse(tokens: List(Token)) -> List(Stmt) {
  case do_parse([], tokens) {
    Ok(#(statements, [Token(Eof, ..)])) | Ok(#(statements, [])) -> list.reverse(statements)
    Ok(#(statements, [Token(line: line, ..)] as tokens)) ->
      error.handle_error(ParseError(
        message: "unexpected end of tokens.",
        exprs: [],
        line: line,
        stmts: statements,
        tokens: tokens,
      ))
    Ok(#(statements, tokens)) ->
      error.handle_error(ParseError(
        message: "parser failed to parse all tokens.",
        exprs: [],
        line: 0,
        stmts: statements,
        tokens: tokens,
      ))
    Error(error) -> error.handle_error(error)
  }
}

fn do_parse(statements, tokens) -> LoxResult(StmtsAndTokens) {
  case statement(Ok(#(statements, tokens))) {
    Ok(#(new_statements, [Token(Eof, ..)])) -> Ok(#(new_statements, []))
    Ok(#(new_statements, [Token(line: line, ..)] as tokens)) -> {
      Error(ParseError(
        message: "BOLO DE FOOBAR unexpected end of tokens.",
        exprs: [],
        line: line,
        stmts: new_statements,
        tokens: tokens,
      ))
    }
    Ok(#(new_statements, remaining_tokens)) ->
      statement(Ok(#(new_statements, remaining_tokens)))
    Error(error) -> handle_error(error)
  }
}

// Catch as many parse errors as we can in one fell swoop.
fn handle_error(error) {
  case error {
    ParseError(
      message: message,
      line: line,
      exprs: exprs,
      tokens: tokens,
      stmts: statements,
    ) -> {
      case tokens {
        [] -> Error(error)
        _ -> {
          let assert Ok(#(new_expr, other_tokens)) = expression(tokens)
          handle_error(ParseError(
            message: message,
            line: line,
            exprs: [new_expr, ..exprs],
            stmts: statements,
            tokens: other_tokens,
          ))
        }
      }
    }
    _ -> Error(error)
  }
}

fn statement(
  stmts_and_tokens: LoxResult(StmtsAndTokens),
) -> LoxResult(StmtsAndTokens) {
  case stmts_and_tokens {
    Ok(#(statements, [])) -> Ok(#(statements, []))
    Ok(#(statements, [one_token])) -> {
      Error(ParseError(
        message: "you might be missing a semicolon.",
        exprs: [],
        line: 0,
        stmts: statements,
        tokens: [one_token],
      ))
    }
    Ok(#(statements, [first_token, ..other_tokens] as tokens)) ->
      case first_token.token_type {
        Print -> {
          let assert Ok(#(expr, [_semicolon, ..new_tokens])) =
            expression(other_tokens)
          let statement = PrintStmt(expression: expr)
          Ok(#([statement, ..statements], new_tokens))
        }
        _ -> {
          case expression(tokens) {
            Ok(#(expr, [_semicolon, ..new_tokens])) -> {
              let statement = ExprStmt(expression: expr)
              Ok(#([statement, ..statements], new_tokens))
            }
            Error(error) -> Error(error)
          }
        }
      }
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
            stmts: [],
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
        stmts: [],
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
