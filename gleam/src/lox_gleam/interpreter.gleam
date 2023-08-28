//// Expose `interpret`, which takes an abstract syntax tree and
//// processes it accoding to the rules of the Lox language.

import gleam/dynamic.{Dynamic}
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/pair
import gleam/result.{then}
import gleam/string
import lox_gleam/ast_types.{
  Assign, Binary, Block, Call, ExprStmt, FunDecl, Grouping, IfStmt, Literal,
  Logical, LoxFunction, PrintStmt, Stmt, Unary, VarDecl, Variable, WhileStmt,
}
import lox_gleam/environment.{Environment, Local}
import lox_gleam/error.{LoxResult, NotImplementedError, RuntimeError}
import lox_gleam/error_handler
import lox_gleam/token
import lox_gleam/token_type.{
  And, Bang, BangEqual, EqualEqual, Greater, GreaterEqual, Less, LessEqual,
  Minus, Or, Plus, Slash, Star, TokenType,
}

pub fn interpret(statements: List(Stmt), environment) -> Environment {
  case execute(statements, environment) {
    Ok(#([], new_environment)) -> new_environment
    Error(error) -> error_handler.handle_error(error)
  }
}

fn execute(statements, environment) {
  do_execute(statements, environment)
  |> then(fn(result) {
    case result {
      #([], env) -> Ok(#([], env))
      #(new_statements, new_environment) ->
        do_execute(new_statements, new_environment)
    }
  })
}

fn do_execute(statements, environment) -> LoxResult(#(List(Stmt), Environment)) {
  case statements {
    [] -> Ok(#([], environment))
    _ -> {
      let [statement, ..other_statements] = statements
      case statement {
        Block(block_statements) -> {
          block(block_statements, environment)
          |> then(fn(new_environment) {
            execute(other_statements, new_environment)
          })
        }
        ExprStmt(expression: expression) ->
          expression_stmt(expression, other_statements, environment)
        FunDecl(..) -> fun_declaration(statement, other_statements, environment)
        IfStmt(..) -> if_stmt(statement, other_statements, environment)
        PrintStmt(expression: expression) ->
          print_stmt(expression, other_statements, environment)
        VarDecl(name, initializer) ->
          variable_declaration(name, initializer, other_statements, environment)
        WhileStmt(condition, body) ->
          while_stmt(condition, body, other_statements, environment)
      }
    }
  }
}

fn fun_declaration(fun_decl, other_statements, environment) {
  let assert FunDecl(name: name_token, params: params, ..) = fun_decl
  let function =
    LoxFunction(
      arity: list.length(params),
      to_string: "<fn " <> name_token.lexeme <> ">",
      declaration: fun_decl,
    )
  let environment1 =
    environment.define(environment, name_token.lexeme, dynamic.from(function))
  do_execute(other_statements, environment1)
}

fn if_stmt(
  if_statement,
  other_statements,
  environment,
) -> LoxResult(#(List(Stmt), Environment)) {
  let assert IfStmt(condition, then_branch, else_branch) = if_statement
  evaluate(condition, environment)
  |> then(fn(result) {
    let #(value, new_environment) = result
    case is_truthy(value) {
      True -> do_then_branch(then_branch, other_statements, new_environment)
      False -> do_else_branch(else_branch, other_statements, new_environment)
    }
  })
}

fn do_then_branch(statement, other_statements, environment) {
  [statement]
  |> execute(environment)
  |> result.then(fn(result) {
    case result {
      #([], new_environment) -> execute(other_statements, new_environment)
      _ -> Error(NotImplementedError)
    }
  })
}

fn do_else_branch(else_branch, other_statements, environment) {
  case else_branch {
    Some(statement) -> {
      execute([statement], environment)
      |> then(fn(result) {
        case result {
          #([], new_environment) -> execute(other_statements, new_environment)
          _ -> Error(NotImplementedError)
        }
      })
    }
    None -> execute(other_statements, environment)
  }
}

fn block(block_statements, environment) {
  environment
  |> Some()
  |> environment.create()
  |> execute(block_statements, _)
  |> then(fn(result) {
    case result {
      #([], Local(parent: new_parent, ..)) -> Ok(new_parent)
      _ -> Error(NotImplementedError)
    }
  })
}

fn expression_stmt(expression, statements, environment) {
  evaluate(expression, environment)
  |> then(fn(result) {
    let #(_value, new_environment) = result
    do_execute(statements, new_environment)
  })
}

fn print_stmt(expression, statements, environment) {
  evaluate(expression, environment)
  |> then(fn(result) {
    let #(value, new_environment) = result

    value
    |> string.inspect()
    |> io.println()
    do_execute(statements, new_environment)
  })
}

fn variable_declaration(name_token, initializer, statements, environment) {
  evaluate(initializer, environment)
  |> then(fn(result) {
    let #(value, environment1) = result
    let environment2 =
      environment.define(environment1, name_token.lexeme, value)
    do_execute(statements, environment2)
  })
}

fn while_stmt(condition, body, other_statements, environment) {
  evaluate(condition, environment)
  |> then(fn(result) {
    let #(value, environment1) = result
    case is_truthy(value) {
      True -> do_while_stmt(condition, body, other_statements, environment1)
      False -> execute(other_statements, environment1)
    }
  })
}

fn do_while_stmt(condition, body, other_statements, environment) {
  execute([body], environment)
  |> then(fn(result) {
    let #(_stmt, new_environment) = result
    while_stmt(condition, body, other_statements, new_environment)
  })
}

fn evaluate(expression, environment) -> LoxResult(#(Dynamic, Environment)) {
  case expression {
    Assign(name: name_token, value: expr) ->
      evaluate_assignment(name_token, expr, environment)
    Binary(operator, left, right, ..) ->
      evaluate_binary(operator, left, right, environment)
    Call(..) -> evaluate_call(expression, environment)
    Grouping(expression, ..) -> evaluate(expression, environment)
    Literal(value, ..) -> Ok(#(value, environment))
    Logical(operator, left, right, ..) ->
      evaluate_logical(operator, left, right, environment)
    Unary(operator, right, ..) -> evaluate_unary(operator, right, environment)
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
  evaluate(expression, environment)
  |> then(fn(result) { do_assignment(name_token, result) })
}

fn do_assignment(name_token, result) {
  let #(value, environment) = result
  environment.assign(environment, name_token, dynamic.from(value))
  |> then(fn(new_environment) { Ok(#(dynamic.from(value), new_environment)) })
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

fn evaluate_call(call_expr, environment) -> LoxResult(#(Dynamic, Environment)) {
  let assert Call(callee, _paren, arguments) = call_expr
  evaluate(callee, environment)
  |> then(fn(result) {
    let #(callee_value, environment1) = result
    let initial_env = Ok(#([], environment1))
    list.fold(arguments, initial_env, evaluate_arguments)
    |> result.map(fn(args_and_environment) {
      let #(args, env) = args_and_environment
      #(callee_value, args, env)
    })
  })
  |> then(call_function)
}

fn evaluate_arguments(accumulator, argument) {
  let assert Ok(#(current_args, current_env)) = accumulator
  let assert Ok(#(argument_value, environment2)) =
    evaluate(argument, current_env)
  Ok(#([argument_value, ..current_args], environment2))
}

fn call_function(result) {
  let #(callee_value, argument_values, environment2) = result
  // Check that `callee_value` is callable
  let decoded_callee = dynamic.unsafe_coerce(callee_value)
  case decoded_callee {
    LoxFunction(arity: arity, declaration: declaration, ..) -> {
      let assert FunDecl(params: params, body: body, ..) = declaration
      let args_length = list.length(argument_values)
      case args_length == arity {
        True -> do_call_function(params, argument_values, body, environment2)
        False ->
          Error(RuntimeError(
            message: "Expected " <> string.inspect(arity) <> " arguments but got " <> string.inspect(
              args_length,
            ),
            values: [],
          ))
      }
    }
    _ ->
      Error(RuntimeError(
        message: "Can only call functions and classes.",
        values: [],
      ))
  }
}

fn do_call_function(params: List(token.Token), arguments, body, environment) {
  let fun_environment = environment.create(Some(environment))
  params
  |> list.zip(with: arguments)
  |> list.fold(
    from: fun_environment,
    with: fn(current_env, param_arg_pair) {
      environment.define(
        current_env,
        pair.first(param_arg_pair).lexeme,
        pair.second(param_arg_pair),
      )
    },
  )
  |> block(body, _)

  Ok(#(dynamic.from(Nil), environment))
}

fn evaluate_logical(operator, left_expr, right_expr, environment) {
  let assert Ok(#(left_value, environment1)) = evaluate(left_expr, environment)
  case operator, is_truthy(left_value) {
    Or, True | And, False -> Ok(#(dynamic.from(left_value), environment1))
    And, True | Or, False -> {
      let assert Ok(#(right_value, environment2)) =
        evaluate(right_expr, environment1)
      Ok(#(dynamic.from(right_value), environment2))
    }
    _, _ -> Error(NotImplementedError)
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
