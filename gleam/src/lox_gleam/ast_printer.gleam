import gleam/string
import gleam/string_builder
import lox_gleam/ast_types
import lox_gleam/token_type

pub fn print(expr: ast_types.Expr) {
  expr
  |> build_string
  |> string_builder.to_string()
}

fn build_string(expr) {
  case expr {
    ast_types.Binary(operator, left, right, _line) ->
      print_binary(operator, left, right)
    ast_types.Grouping(expression, _line) -> print_grouping(expression)
    ast_types.Literal(value, _line) ->
      string_builder.from_string(" " <> string.inspect(value))
    ast_types.Unary(operator, right, _line) -> print_unary(operator, right)
    ast_types.Variable(name) -> string_builder.from_string(name)
  }
}

fn print_binary(operator, left, right) {
  string_builder.concat([
    string_builder.from_string("(" <> stringify(operator)),
    build_string(left),
    build_string(right),
    string_builder.from_string(")"),
  ])
}

fn print_grouping(expr) {
  string_builder.concat([
    string_builder.from_string("(group"),
    build_string(expr),
    string_builder.from_string(")"),
  ])
}

fn print_unary(operator, right) {
  string_builder.concat([
    string_builder.from_string("(" <> stringify(operator)),
    build_string(right),
    string_builder.from_string(")"),
  ])
}

fn stringify(operator) {
  case operator {
    token_type.Bang -> "!"
    token_type.BangEqual -> "!="
    token_type.EqualEqual -> "=="
    token_type.Greater -> ">"
    token_type.GreaterEqual -> ">="
    token_type.Less -> "<"
    token_type.LessEqual -> "<="
    token_type.Minus -> "-"
    token_type.Plus -> "+"
    token_type.Slash -> "/"
    token_type.Star -> "*"

    _ -> ""
  }
}
