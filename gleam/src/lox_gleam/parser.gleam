import gleam/dynamic
import gleam/list
import lox_gleam/ast_types.{Binary, Expr, Grouping, Literal, Unary}
import lox_gleam/token.{Token}
import lox_gleam/token_type.{
  Bang, BangEqual, Eof, EqualEqual, Greater, GreaterEqual, LeftParen, Less,
  LessEqual, LoxFalse, LoxNil, LoxString, LoxTrue, Minus, Number, Plus, Slash,
  Star,
}

pub fn parse(tokens: List(Token)) {
  let #(_eof, [expr, .._exprs]) = expression(tokens, [Expr])
  expr
}

fn expression(tokens, exprs) -> #(List(Token), List(Expr)) {
  equality(tokens, exprs)
}

fn equality(tokens, exprs) {
  parse_binary(tokens, exprs, [BangEqual, EqualEqual], comparison)
}

fn comparison(tokens, exprs) {
  parse_binary(tokens, exprs, [Less, LessEqual, Greater, GreaterEqual], term)
}

fn term(tokens, exprs) {
  parse_binary(tokens, exprs, [Plus, Minus], factor)
}

fn factor(tokens, exprs) {
  parse_binary(tokens, exprs, [Slash, Star], unary)
}

fn unary(tokens: List(Token), exprs) {
  let [first_token, ..other_tokens] = tokens
  let is_unary_operator =
    [Bang, Minus]
    |> list.any(match(_, first_token))
  case is_unary_operator {
    True -> parse_unary(other_tokens, exprs, first_token)
    False -> primary(tokens, exprs)
  }
}

fn primary(tokens: List(Token), exprs) {
  let [first_token, ..other_tokens] = tokens
  case first_token.token_type {
    LoxFalse -> build_literal(dynamic.from(False), other_tokens, exprs)
    LoxTrue -> build_literal(dynamic.from(True), other_tokens, exprs)
    LoxNil -> build_literal(dynamic.from(Nil), other_tokens, exprs)
    LoxString | Number ->
      build_literal(dynamic.from(first_token.literal), other_tokens, exprs)
    LeftParen -> {
      let #([_right_paren, ..new_tokens], [inner_expr, ..new_exprs]) =
        expression(other_tokens, exprs)
      let expr = Grouping(expression: inner_expr)
      expression(new_tokens, [expr, ..new_exprs])
    }
    _ -> #([Token(Eof, "", dynamic.from(Nil), 1)], exprs)
  }
}

fn build_literal(value, other_tokens, exprs) {
  let expr = Literal(value: value)
  #(other_tokens, [expr, ..exprs])
}

fn parse_binary(tokens: List(Token), exprs, types, function) {
  let [first_token, ..other_tokens] = tokens
  let is_operator =
    types
    |> list.any(match(_, first_token))
  case is_operator {
    True -> {
      let #(latest_expr, earlier_exprs) = case exprs {
        [] -> #(Expr, [])
        [expr, ..earlier_exprs] -> #(expr, earlier_exprs)
      }
      case match(Minus, first_token), latest_expr {
        True, Literal(value) -> {
          case dynamic.float(value) {
            Ok(_) ->
              parse_binary_operator(
                other_tokens,
                latest_expr,
                first_token,
                earlier_exprs,
                function,
              )
            // This is an edge case: if we get here, the minus sign is
            // being used as a unary operator.
            Error(_) -> unary(tokens, exprs)
          }
        }
        // This is also a case of unary minus.
        True, _ -> unary(tokens, exprs)
        False, _ ->
          parse_binary_operator(
            other_tokens,
            latest_expr,
            first_token,
            earlier_exprs,
            function,
          )
      }
    }
    False -> function(tokens, exprs)
  }
}

fn match(token_type, token: Token) {
  token.token_type == token_type
}

fn parse_binary_operator(tokens, left, first, exprs, function) {
  let #(new_tokens, [right_expr, ..other_exprs]) = function(tokens, exprs)
  let expr = Binary(left: left, operator: first, right: right_expr)
  #(new_tokens, [expr, ..other_exprs])
}

fn parse_unary(tokens, exprs, operator) {
  let #(new_tokens, [right_expr, ..other_exprs]) = unary(tokens, exprs)
  let expr = Unary(operator: operator, right: right_expr)
  #(new_tokens, [expr, ..other_exprs])
}
