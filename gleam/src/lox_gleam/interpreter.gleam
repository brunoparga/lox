//// Expose `interpret`, which takes an abstract syntax tree and
//// processes it accoding to the rules of the Lox language. Initially,
//// since the AST contains only expressions, all the interpreter does
//// is calculate the value of the expression.

import gleam/dynamic.{Dynamic}
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import lox_gleam/ast_types.{
  Binary, ExprStmt, Grouping, Literal, PrintStmt, Stmt, Unary,
}
import lox_gleam/error.{LoxError, LoxResult, RuntimeError}
import lox_gleam/token_type.{
  Bang, BangEqual, EqualEqual, Greater, GreaterEqual, Less, LessEqual, Minus,
  Plus, Slash, Star, TokenType,
}

pub fn interpret(statements: List(Stmt)) -> List(Stmt) {
  case execute(statements) {
    Ok([]) -> []
    Error(error) -> error.handle_error(error)
  }
}

fn execute(statements) {
  case do_execute(statements) {
    Ok([]) -> Ok([])
    Ok(new_statements) -> do_execute(new_statements)
    Error(error) -> Error(error)
  }
}

fn do_execute(statements) -> LoxResult(List(Stmt)) {
  case statements {
    [] -> Ok([])
    [statement, ..new_statements] ->
      case statement {
        ExprStmt(expression: expression) -> {
          case evaluate(expression) {
            Ok(_value) -> do_execute(new_statements)
            Error(error) -> Error(error)
          }
        }
        PrintStmt(expression: expression) -> {
          case evaluate(expression) {
            Ok(value) -> {
              value
              |> string.inspect()
              |> io.println()
              do_execute(new_statements)
            }
            Error(error) -> Error(error)
          }
        }
      }
  }
}

fn evaluate(expression) -> Result(Dynamic, LoxError) {
  case expression {
    Literal(value, ..) -> Ok(value)
    Grouping(expression, ..) -> evaluate(expression)
    Unary(operator, right, ..) -> evaluate_unary(operator, right)
    Binary(operator, left, right, ..) -> evaluate_binary(operator, left, right)
    _ ->
      Error(RuntimeError(message: "unexpected expression found.", values: []))
  }
}

fn evaluate_unary(operator: TokenType, right_expr) -> Result(Dynamic, LoxError) {
  let assert Ok(value) = evaluate(right_expr)
  case operator {
    Bang -> Ok(dynamic.from(!is_truthy(value)))
    Minus -> {
      let assert Ok(number) = dynamic.float(value)
      Ok(dynamic.from(0.0 -. number))
    }
    _ -> Error(RuntimeError(message: "", values: [value]))
  }
}

fn evaluate_binary(operator, left_expr, right_expr) -> Result(Dynamic, LoxError) {
  let assert Ok(left_value) = evaluate(left_expr)
  let assert Ok(right_value) = evaluate(right_expr)
  // Some binary operators take only certain types.
  let numbers = unpack_values(dynamic.float, left_value, right_value)
  let strings = unpack_values(dynamic.string, left_value, right_value)
  // Evaluate the binary operators.
  case operator {
    BangEqual -> Ok(dynamic.from(!{ left_value == right_value }))
    EqualEqual -> Ok(dynamic.from({ left_value == right_value }))
    Greater -> {
      case numbers {
        Some([left_number, right_number]) ->
          Ok(dynamic.from(left_number >. right_number))
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
          Ok(dynamic.from(left_number >=. right_number))
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
          Ok(dynamic.from(left_number <. right_number))
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
          Ok(dynamic.from(left_number <=. right_number))
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
          Ok(dynamic.from(left_number -. right_number))
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
          Ok(dynamic.from(left_number +. right_number))
        None, Some([left_string, right_string]) ->
          Ok(dynamic.from(left_string <> right_string))
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
          Ok(dynamic.from(left_number /. right_number))
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
          Ok(dynamic.from(left_number *. right_number))
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
