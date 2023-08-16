//// Expose `parse`, a function that takes in a list of Lox tokens and
//// returns an abstract syntax tree.

import gleam/dynamic
import gleam/list
import lox_gleam/ast_types.{
  Assign, Binary, Block, Expr, ExprStmt, Grouping, Literal, PrintStmt, Stmt,
  Unary, VarStmt, Variable,
}
import lox_gleam/error.{LoxResult, ParseError}
import lox_gleam/error_handler
import lox_gleam/token.{Token}
import lox_gleam/token_type.{
  Bang, BangEqual, Eof, Equal, EqualEqual, Greater, GreaterEqual, Identifier,
  LeftBrace, LeftParen, Less, LessEqual, LoxFalse, LoxNil, LoxString, LoxTrue,
  Minus, Number, Plus, Print, RightBrace, Semicolon, Slash, Star, TokenType, Var,
}

type ExprAndTokens =
  #(Expr, List(Token))

type StmtsAndTokens =
  #(List(Stmt), List(Token))

pub fn parse(tokens: List(Token)) -> List(Stmt) {
  case declaration(Ok(#([], tokens))) {
    Ok(#(statements, [])) -> list.reverse(statements)
    Ok(#(statements, [Token(line: line, ..)] as tokens)) -> {
      error_handler.handle_error(ParseError(
        message: "unexpected end of tokens.",
        exprs: [],
        line: line,
        stmts: statements,
        tokens: tokens,
      ))
      []
    }
    Ok(#(statements, tokens)) -> {
      error_handler.handle_error(ParseError(
        message: "parser failed to parse all tokens.",
        exprs: [],
        line: 0,
        stmts: statements,
        tokens: tokens,
      ))
      []
    }
    Error(error) -> {
      error_handler.handle_error(error)
      []
    }
  }
}

fn declaration(stmts_and_tokens: LoxResult(StmtsAndTokens)) {
  case stmts_and_tokens {
    Ok(#(statements, [Token(Eof, ..)])) -> Ok(#(statements, []))
    Ok(#(statements, [one_token])) -> {
      Error(ParseError(
        message: "you might be missing a semicolon.",
        exprs: [],
        line: 0,
        stmts: statements,
        tokens: [one_token],
      ))
    }
    Ok(#(statements, tokens)) -> statement(statements, tokens)
    Error(error) -> group_errors(error)
  }
}

fn statement(statements, tokens: List(Token)) -> LoxResult(StmtsAndTokens) {
  let [first_token, ..other_tokens] = tokens
  case first_token.token_type {
    Var -> var_declaration(statements, other_tokens)
    LeftBrace -> block(statements, other_tokens)
    RightBrace -> Ok(#(statements, other_tokens))
    Print -> do_statement(statements, other_tokens, PrintStmt)
    _ -> do_statement(statements, tokens, ExprStmt)
  }
}

fn var_declaration(statements, tokens: List(Token)) {
  let [variable_name, ..tokens1] = tokens
  case variable_name.token_type {
    Identifier -> do_var_declaration(variable_name, statements, tokens1)
    _ ->
      Error(ParseError(
        message: "expect variable name.",
        line: variable_name.line,
        exprs: [],
        stmts: statements,
        tokens: tokens,
      ))
  }
}

fn do_var_declaration(variable_name: Token, statements, tokens: List(Token)) {
  let [maybe_equal, ..new_tokens] = tokens
  case maybe_equal.token_type {
    Equal ->
      var_declaration_with_assignment(variable_name, statements, new_tokens)
    Semicolon -> {
      let nil_expr = Literal(dynamic.from(Nil), variable_name.line)
      let var_statement = VarStmt(name: variable_name, initializer: nil_expr)
      declaration(Ok(#([var_statement, ..statements], new_tokens)))
    }
    _ ->
      Error(ParseError(
        message: "a variable declaration must be followed by an assignment or ';'.",
        line: maybe_equal.line,
        exprs: [],
        stmts: statements,
        tokens: tokens,
      ))
  }
}

fn var_declaration_with_assignment(name, statements, tokens: List(Token)) {
  case expression(tokens) {
    Ok(#(expr, [semicolon, ..new_tokens])) -> {
      case semicolon.token_type {
        Semicolon -> {
          let var_statement = VarStmt(name: name, initializer: expr)
          declaration(Ok(#([var_statement, ..statements], new_tokens)))
        }
        _ ->
          Error(ParseError(
            message: "a variable declaration with assignment must be followed by ';'.",
            line: semicolon.line,
            exprs: [],
            stmts: statements,
            tokens: new_tokens,
          ))
      }
    }
    Error(error) -> Error(error)
  }
}

fn block(existing_statements, tokens: List(Token)) -> LoxResult(StmtsAndTokens) {
  let [first_token, ..other_tokens] = tokens
  case first_token.token_type {
    Eof -> Error(error.NotImplementedError)
    _ -> {
      case declaration(Ok(#([], other_tokens))) {
        Ok(#(block_statements, new_tokens)) -> {
          let block = Block(list.reverse(block_statements))
          declaration(Ok(#([block, ..existing_statements], new_tokens)))
        }
        Error(error) -> Error(error)
      }
    }
  }
}

fn do_statement(statements, tokens, stmt_type) {
  case expression(tokens) {
    Ok(#(expr, [Token(token_type: Semicolon, ..), ..new_tokens])) -> {
      let new_statement = stmt_type(expr)
      declaration(Ok(#([new_statement, ..statements], new_tokens)))
    }
    Error(error) -> Error(error)
  }
}

fn expression(tokens) -> LoxResult(ExprAndTokens) {
  assignment(tokens)
}

fn assignment(tokens) -> LoxResult(ExprAndTokens) {
  case equality(tokens) {
    Ok(#(name_expr, [])) ->
      Error(ParseError(
        message: "unexpected end of tokens when assigning variable.",
        exprs: [name_expr],
        line: 0,
        stmts: [],
        tokens: tokens,
      ))
    Ok(#(name_expr, new_tokens)) -> do_assignment(name_expr, new_tokens)
    Error(error) -> Error(error)
  }
}

fn do_assignment(name_expr, tokens) -> LoxResult(ExprAndTokens) {
  let [equals, ..tokens1] = tokens
  case equals.token_type {
    Equal -> {
      case assignment(tokens1) {
        Ok(#(value_expr, tokens2)) -> {
          case name_expr {
            Variable(name: name) -> {
              Ok(#(Assign(name: name, value: value_expr), tokens2))
            }
            _ ->
              Error(ParseError(
                message: "invalid assignment target.",
                exprs: [name_expr],
                stmts: [],
                tokens: tokens,
                line: equals.line,
              ))
          }
        }
        Error(error) -> Error(error)
      }
    }
    _ -> Ok(#(name_expr, tokens))
  }
}

fn equality(tokens) -> LoxResult(ExprAndTokens) {
  tokens
  |> comparison()
  |> binary_inner([BangEqual, EqualEqual], comparison)
}

fn comparison(tokens) -> LoxResult(ExprAndTokens) {
  tokens
  |> term()
  |> binary_inner([Greater, GreaterEqual, Less, LessEqual], term)
}

fn term(tokens) -> LoxResult(ExprAndTokens) {
  tokens
  |> factor()
  |> binary_inner([Minus, Plus], factor)
}

fn factor(tokens) -> LoxResult(ExprAndTokens) {
  tokens
  |> unary()
  |> binary_inner([Slash, Star], unary)
}

fn unary(tokens) -> LoxResult(ExprAndTokens) {
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

fn primary(tokens: List(Token)) -> LoxResult(ExprAndTokens) {
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
    Identifier -> Ok(#(Variable(name: first_token), other_tokens))
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
  fn(expr_and_tokens) -> LoxResult(ExprAndTokens) {
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

// Catch as many parse errors as we can in one fell swoop.
fn group_errors(error) {
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
          group_errors(ParseError(
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
