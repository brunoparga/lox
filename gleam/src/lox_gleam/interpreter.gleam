//// Expose `interpret`, which takes an abstract syntax tree and
//// processes it accoding to the rules of the Lox language.

import gleam/dynamic.{Dynamic}
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import lox_gleam/ast_types.{
  Assign, Binary, Block, ExprStmt, Grouping, Literal, PrintStmt, Stmt, Unary,
  VarStmt, Variable,
}
import lox_gleam/environment.{Environment}
import lox_gleam/error.{LoxResult, RuntimeError}
import lox_gleam/error_handler
import lox_gleam/token_type.{
  Bang, BangEqual, EqualEqual, Greater, GreaterEqual, Less, LessEqual, Minus,
  Plus, Slash, Star, TokenType,
}

pub fn interpret(statements: List(Stmt), environment) -> Environment {
  case execute(statements, environment) {
    Ok(#([], new_environment)) -> new_environment
    Error(error) -> error_handler.handle_error(error)
  }
}

fn execute(statements, environment) {
  case do_execute(statements, environment) {
    Ok(#([], env)) -> Ok(#([], env))
    Ok(#(new_statements, environment)) ->
      do_execute(new_statements, environment)
    Error(error) -> Error(error)
  }
}

fn do_execute(statements, environment) -> LoxResult(#(List(Stmt), Environment)) {
  case statements {
    [] -> Ok(#([], environment))
    _ -> {
      let [statement, ..other_statements] = statements
      case statement {
        ExprStmt(expression: expression) ->
          expression_stmt(expression, other_statements, environment)
        PrintStmt(expression: expression) ->
          print_stmt(expression, other_statements, environment)
        VarStmt(name, initializer) ->
          variable_stmt(name, initializer, other_statements, environment)
        Block(..) -> Error(error.NotImplementedError)
      }
    }
  }
}

fn expression_stmt(expression, statements, environment) {
  case evaluate(expression, environment) {
    Ok(#(_value, new_environment)) -> do_execute(statements, new_environment)
    Error(error) -> Error(error)
  }
}

fn print_stmt(expression, statements, environment) {
  case evaluate(expression, environment) {
    Ok(#(value, new_environment)) -> {
      value
      |> string.inspect()
      |> io.println()
      do_execute(statements, new_environment)
    }
    Error(error) -> Error(error)
  }
}

fn variable_stmt(name_token, initializer, statements, environment) {
  case evaluate(initializer, environment) {
    Ok(#(value, environment1)) -> {
      let environment2 =
        environment.define(environment1, name_token.lexeme, value)
      do_execute(statements, environment2)
    }
    Error(error) -> Error(error)
  }
}

fn evaluate(expression, environment) -> LoxResult(#(Dynamic, Environment)) {
  case expression {
    Literal(value, ..) -> Ok(#(value, environment))
    Assign(name: name_token, value: expr) ->
      evaluate_assignment(name_token, expr, environment)
    Grouping(expression, ..) -> evaluate(expression, environment)
    Unary(operator, right, ..) -> evaluate_unary(operator, right, environment)
    Binary(operator, left, right, ..) ->
      evaluate_binary(operator, left, right, environment)
    Variable(name_token) -> environment.get(environment, name_token.lexeme)
    _ ->
      Error(RuntimeError(message: "unexpected expression found.", values: []))
  }
}

fn evaluate_assignment(
  name_token,
  expression,
  environment,
) -> LoxResult(#(Dynamic, Environment)) {
  case evaluate(expression, environment) {
    Ok(#(value, environment1)) -> {
      case environment.assign(environment1, name_token, dynamic.from(value)) {
        Ok(environment2) -> Ok(#(dynamic.from(value), environment2))
        Error(error) -> Error(error)
      }
    }
    Error(error) -> Error(error)
  }
}

fn evaluate_unary(
  operator: TokenType,
  right_expr,
  environment,
) -> LoxResult(#(Dynamic, Environment)) {
  let assert Ok(#(value, new_environment)) = evaluate(right_expr, environment)
  case operator {
    Bang -> Ok(#(dynamic.from(!is_truthy(value)), new_environment))
    Minus -> {
      let assert Ok(number) = dynamic.float(value)
      Ok(#(dynamic.from(0.0 -. number), new_environment))
    }
    _ ->
      Error(RuntimeError(
        message: "unexpected operator in unary expression.",
        values: [value],
      ))
  }
}

fn evaluate_binary(
  operator,
  left_expr,
  right_expr,
  environment,
) -> LoxResult(#(Dynamic, Environment)) {
  let assert Ok(#(left_value, environment1)) = evaluate(left_expr, environment)
  let assert Ok(#(right_value, environment2)) =
    evaluate(right_expr, environment1)
  // Some binary operators take only certain types.
  let numbers = unpack_values(dynamic.float, left_value, right_value)
  let strings = unpack_values(dynamic.string, left_value, right_value)
  // Evaluate the binary operators.
  case operator {
    BangEqual ->
      Ok(#(dynamic.from(!{ left_value == right_value }), environment2))
    EqualEqual ->
      Ok(#(dynamic.from({ left_value == right_value }), environment2))
    Greater -> {
      case numbers {
        Some([left_number, right_number]) ->
          Ok(#(dynamic.from(left_number >. right_number), environment2))
        None ->
          Error(RuntimeError(
            message: "binary operator " <> string.inspect(operator) <> " takes two numbers.",
            values: [left_value, right_value],
          ))
      }
    }
    GreaterEqual -> {
      case numbers {
        Some([left_number, right_number]) ->
          Ok(#(dynamic.from(left_number >=. right_number), environment2))
        None ->
          Error(RuntimeError(
            message: "binary operator " <> string.inspect(operator) <> " takes two numbers.",
            values: [left_value, right_value],
          ))
      }
    }
    Less -> {
      case numbers {
        Some([left_number, right_number]) ->
          Ok(#(dynamic.from(left_number <. right_number), environment2))
        None ->
          Error(RuntimeError(
            message: "binary operator " <> string.inspect(operator) <> " takes two numbers.",
            values: [left_value, right_value],
          ))
      }
    }
    LessEqual -> {
      case numbers {
        Some([left_number, right_number]) ->
          Ok(#(dynamic.from(left_number <=. right_number), environment2))
        None ->
          Error(RuntimeError(
            message: "binary operator " <> string.inspect(operator) <> " takes two numbers.",
            values: [left_value, right_value],
          ))
      }
    }
    Minus -> {
      case numbers {
        Some([left_number, right_number]) ->
          Ok(#(dynamic.from(left_number -. right_number), environment2))
        None ->
          Error(RuntimeError(
            message: "binary operator " <> string.inspect(operator) <> " takes two numbers.",
            values: [left_value, right_value],
          ))
      }
    }
    Plus -> {
      case numbers, strings {
        Some([left_number, right_number]), None ->
          Ok(#(dynamic.from(left_number +. right_number), environment2))
        None, Some([left_string, right_string]) ->
          Ok(#(dynamic.from(left_string <> right_string), environment2))
        _, _ ->
          Error(RuntimeError(
            message: "binary operator Plus takes either two numbers or two strings.",
            values: [left_value, right_value],
          ))
      }
    }

    Slash -> {
      case numbers {
        Some([left_number, right_number]) ->
          Ok(#(dynamic.from(left_number /. right_number), environment2))
        None ->
          Error(RuntimeError(
            message: "binary operator " <> string.inspect(operator) <> " takes two numbers.",
            values: [left_value, right_value],
          ))
      }
    }
    Star -> {
      case numbers {
        Some([left_number, right_number]) ->
          Ok(#(dynamic.from(left_number *. right_number), environment2))
        None ->
          Error(RuntimeError(
            message: "binary operator " <> string.inspect(operator) <> " takes two numbers.",
            values: [left_value, right_value],
          ))
      }
    }
    _ ->
      Error(RuntimeError(
        message: "unrecognized token " <> string.inspect(operator) <> " in binary expression.",
        values: [left_value, right_value],
      ))
  }
}

fn unpack_values(function, left_value, right_value) {
  option.all(list.map(
    [left_value, right_value],
    fn(value) {
      value
      |> function
      |> option.from_result
    },
  ))
}

fn is_truthy(value) -> Bool {
  let is_nil = value == dynamic.from(Nil)
  let is_false = value == dynamic.from(False)
  !is_nil && !is_false
}
