import gleam/dynamic
import gleam/io
import gleam/list
import gleam/string
import gleam/string_builder
import lox_gleam/ast_types
import lox_gleam/token

pub fn print(expr: ast_types.Expr) {
  ast_string_builder(expr)
  |> string_builder.to_string()
  |> string.trim_left()
  |> io.println()
}

fn ast_string_builder(expr) {
  case expr {
    ast_types.Binary(left, operator, right) ->
      print_binary(left, operator, right)
    ast_types.Expr -> string_builder.from_string("")
    ast_types.Grouping(expression) -> print_grouping(expression)
    ast_types.Literal(value) -> print_literal(value)
    ast_types.Unary(operator, right) -> print_unary(operator, right)
  }
}

fn print_binary(left, operator: token.Token, right) {
  parenthesize(operator.lexeme, [left, right])
}

fn print_grouping(expression: ast_types.Expr) {
  parenthesize("group" <> " ", [expression])
}

fn print_literal(value: dynamic.Dynamic) {
  value
  |> string.inspect()
  |> string_builder.from_string()
}

fn print_unary(operator: token.Token, right) {
  parenthesize(operator.lexeme, [right])
}

fn parenthesize(name, expressions) {
  string_builder.new()
  |> string_builder.append(" ")
  |> string_builder.append("(")
  |> string_builder.append(name)
  |> string_builder.append_builder(recurse(expressions))
  |> string_builder.append(")")
}

fn recurse(expressions) {
  expressions
  |> list.map(ast_string_builder)
  |> string_builder.concat()
}
