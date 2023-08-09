//// Expose `interpret`, which takes an abstract syntax tree and
//// processes it accoding to the rules of the Lox language. Initially,
//// since the AST contains only expressions, all the interpreter does
//// is calculate the value of the expression.

import gleam/dynamic
import gleam/io
import lox_gleam/ast_printer
import lox_gleam/ast_types.{Binary, Grouping, Literal, Unary}
import lox_gleam/token_type.{
  Bang, BangEqual, EqualEqual, Greater, GreaterEqual, Less, LessEqual, Minus,
  Plus, Slash, Star, TokenType,
}

pub fn interpret(expression) {
  case expression {
    Ok(expr) -> {
      ast_printer.print(expr) |> io.println()
      evaluate(expr)
    }
    Error(_reason) -> dynamic.from(Nil)
  }
}

fn evaluate(expression) {
  case expression {
    Literal(value, ..) -> value
    Grouping(expression, ..) -> evaluate(expression)
    Unary(operator, right, ..) -> evaluate_unary(operator, right)
    Binary(operator, left, right, ..) -> evaluate_binary(operator, left, right)
    _ -> dynamic.from(Nil)
  }
}

fn evaluate_unary(operator: TokenType, right_expr) {
  let assert value = evaluate(right_expr)
  case operator {
    Bang -> dynamic.from(!is_truthy(value))
    Minus -> {
      let assert Ok(number) = dynamic.float(value)
      dynamic.from(0.0 -. number)
    }
    // Unreachable, included to satisfy the compiler.
    _ -> dynamic.from(Nil)
  }
}

fn evaluate_binary(operator, left_expr, right_expr) {
  let assert left_value = evaluate(left_expr)
  let assert right_value = evaluate(right_expr)
  // Hey, we decided to implement a dynamic language on top a static one
  // (which itself runs on top of dynamic ones). We brought this upon
  // ourselves, right?
  let #(left_is_number, left_number) = case dynamic.float(left_value) {
    Ok(number) -> #(True, number)
    Error(_) -> #(False, 0.0)
  }
  let #(right_is_number, right_number) = case dynamic.float(right_value) {
    Ok(number) -> #(True, number)
    Error(_) -> #(False, 0.0)
  }
  let #(left_is_string, left_string) = case dynamic.string(left_value) {
    Ok(string) -> #(True, string)
    Error(_) -> #(False, "")
  }
  let #(right_is_string, right_string) = case dynamic.string(right_value) {
    Ok(string) -> #(True, string)
    Error(_) -> #(False, "")
  }
  // Evaluate the binary operators.
  case operator {
    BangEqual -> dynamic.from(!{ left_value == right_value })
    EqualEqual -> dynamic.from({ left_value == right_value })
    Greater -> {
      case left_is_number, right_is_number {
        True, True -> dynamic.from(left_number >. right_number)
        // Throw a RuntimeError later
        _, _ -> dynamic.from(Nil)
      }
    }
    GreaterEqual -> {
      case left_is_number, right_is_number {
        True, True -> dynamic.from(left_number >=. right_number)
        // Throw a RuntimeError later
        _, _ -> dynamic.from(Nil)
      }
    }
    Less -> {
      case left_is_number, right_is_number {
        True, True -> dynamic.from(left_number <. right_number)
        // Throw a RuntimeError later
        _, _ -> dynamic.from(Nil)
      }
    }
    LessEqual -> {
      case left_is_number, right_is_number {
        True, True -> dynamic.from(left_number <=. right_number)
        // Throw a RuntimeError later
        _, _ -> dynamic.from(Nil)
      }
    }
    Minus -> {
      case left_is_number, right_is_number {
        True, True -> dynamic.from(left_number -. right_number)
        // Throw a RuntimeError later
        _, _ -> dynamic.from(Nil)
      }
    }
    Plus -> {
      case left_is_number, right_is_number, left_is_string, right_is_string {
        True, True, False, False -> dynamic.from(left_number +. right_number)
        False, False, True, True -> dynamic.from(left_string <> right_string)
        // Throw a RuntimeError later
        _, _, _, _ -> dynamic.from(Nil)
      }
    }
    Slash -> {
      case left_is_number, right_is_number {
        True, True -> dynamic.from(left_number /. right_number)
        // Throw a RuntimeError later
        _, _ -> dynamic.from(Nil)
      }
    }
    Star -> {
      case left_is_number, right_is_number {
        True, True -> dynamic.from(left_number *. right_number)
        // Throw a RuntimeError later
        _, _ -> dynamic.from(Nil)
      }
    }
    _ -> dynamic.from(Nil)
  }
}

fn is_truthy(value) {
  let is_nil = value == dynamic.from(Nil)
  let is_false = value == dynamic.from(False)
  !is_nil && !is_false
}
