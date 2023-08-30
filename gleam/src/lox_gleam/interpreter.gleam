//// Expose `interpret`, which takes an abstract syntax tree and
//// processes it accoding to the rules of the Lox language.

import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/pair
import gleam/result.{then}
import gleam/string
import lox_gleam/types.{
  And, Assign, Bang, BangEqual, Binary, Block, Call, Environment, EqualEqual,
  Expr, ExprStmt, FunDecl, Greater, GreaterEqual, Grouping, IfStmt, Less,
  LessEqual, Literal, Local, Logical, LoxBool, LoxFunction, LoxNil, LoxNumber,
  LoxString, LoxValue, Minus, NativeFunction, Or, Plus, PrintStmt, ReturnStmt,
  ReturnValue, Slash, Star, Stmt, Token, TokenType, Unary, VarDecl, Variable,
  WhileStmt,
}
import lox_gleam/error.{LoxResult, RuntimeError}
import lox_gleam/error_handler
import lox_gleam/environment

@external(erlang, "os", "system_time")
fn system_time() -> Int

pub fn interpret(
  statements: List(Stmt),
  environment: Environment,
) -> LoxResult(Environment) {
  case execute(statements, environment) {
    Ok(#([], new_environment)) -> Ok(new_environment)
    Error(error) -> error_handler.handle_error(error)
  }
}

fn execute(
  statements: List(Stmt),
  environment: Environment,
) -> LoxResult(#(List(Stmt), Environment)) {
  do_execute(statements, environment)
  |> then(fn(result) {
    case result {
      #([], final_environment) -> Ok(#([], final_environment))
      #(new_statements, new_environment) ->
        do_execute(new_statements, new_environment)
    }
  })
}

fn do_execute(
  statements: List(Stmt),
  environment: Environment,
) -> LoxResult(#(List(Stmt), Environment)) {
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
        ReturnStmt(value: return_expr, ..) ->
          return_stmt(return_expr, environment)
        VarDecl(name, initializer) ->
          variable_declaration(name, initializer, other_statements, environment)
        WhileStmt(condition, body) ->
          while_stmt(condition, body, other_statements, environment)
      }
    }
  }
}

fn block(
  block_statements: List(Stmt),
  environment: Environment,
) -> LoxResult(Environment) {
  environment
  |> Some()
  |> environment.create()
  |> execute(block_statements, _)
  |> then(fn(result) {
    let assert #([], Local(parent: new_parent, ..)) = result
    Ok(new_parent)
  })
}

fn fun_declaration(
  fun_decl: Stmt,
  other_statements: List(Stmt),
  environment: Environment,
) -> LoxResult(#(List(Stmt), Environment)) {
  let assert FunDecl(name: name_token, params: params, ..) = fun_decl
  let function =
    LoxFunction(
      arity: list.length(params),
      to_string: "<fn " <> string.inspect(name_token.value) <> ">",
      closure: environment,
      declaration: fun_decl,
    )
  let environment1 = environment.define(environment, name_token.value, function)
  do_execute(other_statements, environment1)
}

fn if_stmt(
  if_statement: Stmt,
  other_statements: List(Stmt),
  environment: Environment,
) -> LoxResult(#(List(Stmt), Environment)) {
  case environment.get(environment, ReturnValue) {
    Ok(#(_value, environment)) -> execute(other_statements, environment)
    Error(_) -> {
      let assert IfStmt(condition, then_branch, else_branch) = if_statement
      evaluate(condition, environment)
      |> then(fn(result) {
        let #(value, new_environment) = result
        case is_truthy(value), else_branch {
          True, _ ->
            execute_branch(then_branch, other_statements, new_environment)
          False, Some(else_branch) ->
            execute_branch(else_branch, other_statements, new_environment)
          False, None -> execute(other_statements, environment)
        }
      })
    }
  }
}

fn execute_branch(
  statement: Stmt,
  other_statements: List(Stmt),
  environment: Environment,
) -> LoxResult(#(List(Stmt), Environment)) {
  [statement]
  |> execute(environment)
  |> then(fn(result) {
    let assert #([], new_environment) = result
    execute(other_statements, new_environment)
  })
}

fn expression_stmt(
  expression: Expr,
  statements: List(Stmt),
  environment: Environment,
) -> LoxResult(#(List(Stmt), Environment)) {
  evaluate(expression, environment)
  |> then(fn(result) {
    let #(_value, new_environment) = result
    do_execute(statements, new_environment)
  })
}

fn print_stmt(
  expression: Expr,
  statements: List(Stmt),
  environment: Environment,
) -> LoxResult(#(List(Stmt), Environment)) {
  evaluate(expression, environment)
  |> then(fn(result) {
    let #(value, new_environment) = result

    case value {
      LoxBool(bool) ->
        case bool {
          True -> io.println("true")
          False -> io.println("false")
        }
      LoxFunction(to_string: fun_name, ..) -> io.println(fun_name)
      LoxNil -> io.println("nil")
      LoxNumber(number) ->
        case number == float.floor(number) {
          True ->
            number
            |> float.truncate()
            |> string.inspect()
            |> io.println()
          False ->
            number
            |> string.inspect()
            |> io.println()
        }
      LoxString(text) -> io.println(text)
      NativeFunction(to_string: to_string, ..) -> io.println(to_string)
      ReturnValue -> io.println("")
    }

    do_execute(statements, new_environment)
  })
}

fn return_stmt(
  return_expr: Expr,
  environment: Environment,
) -> LoxResult(#(List(Stmt), Environment)) {
  let #(return_value, env1) = case evaluate(return_expr, environment) {
    Ok(result) -> result
    Error(_) -> #(LoxNil, environment)
  }
  let new_environment =
    environment.define_at_global(env1, ReturnValue, return_value)
  Ok(#([], new_environment))
}

fn variable_declaration(
  name_token: Token,
  initializer: Expr,
  statements: List(Stmt),
  environment: Environment,
) -> LoxResult(#(List(Stmt), Environment)) {
  evaluate(initializer, environment)
  |> then(fn(result) {
    let #(value, environment1) = result
    let environment2 = environment.define(environment1, name_token.value, value)
    do_execute(statements, environment2)
  })
}

fn while_stmt(
  condition: Expr,
  body: Stmt,
  other_statements: List(Stmt),
  environment: Environment,
) -> LoxResult(#(List(Stmt), Environment)) {
  evaluate(condition, environment)
  |> then(fn(result) {
    let #(value, environment1) = result
    case is_truthy(value) {
      True -> do_while_stmt(condition, body, other_statements, environment1)
      False -> execute(other_statements, environment1)
    }
  })
}

fn do_while_stmt(
  condition: Expr,
  body: Stmt,
  other_statements: List(Stmt),
  environment: Environment,
) -> LoxResult(#(List(Stmt), Environment)) {
  case environment.get(environment, ReturnValue) {
    Ok(#(_value, environment)) -> execute(other_statements, environment)
    Error(_) -> {
      execute([body], environment)
      |> then(fn(result) {
        let #(_stmt, new_environment) = result
        while_stmt(condition, body, other_statements, new_environment)
      })
    }
  }
}

fn evaluate(
  expression: Expr,
  environment: Environment,
) -> LoxResult(#(LoxValue, Environment)) {
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
    Variable(name_token) -> environment.get(environment, name_token.value)
    _ -> Error(RuntimeError(message: "unexpected expression found."))
  }
}

fn evaluate_assignment(
  name_token: Token,
  expression: Expr,
  environment: Environment,
) -> LoxResult(#(LoxValue, Environment)) {
  evaluate(expression, environment)
  |> then(fn(result) { do_assignment(name_token, result) })
}

fn do_assignment(
  name_token: Token,
  result: #(LoxValue, Environment),
) -> LoxResult(#(LoxValue, Environment)) {
  let #(value, environment) = result
  environment.assign(environment, name_token, value)
  |> then(fn(new_environment) { Ok(#(value, new_environment)) })
}

fn evaluate_binary(
  operator: TokenType,
  left_expr: Expr,
  right_expr: Expr,
  environment: Environment,
) -> LoxResult(#(LoxValue, Environment)) {
  let assert Ok(#(left_value, environment1)) = evaluate(left_expr, environment)
  let assert Ok(#(right_value, environment2)) =
    evaluate(right_expr, environment1)
  let numbers = case left_value, right_value {
    LoxNumber(left), LoxNumber(right) -> Some(#(left, right))
    _, _ -> None
  }
  let strings = case left_value, right_value {
    LoxString(left), LoxString(right) -> Some(#(left, right))
    _, _ -> None
  }
  // Evaluate the binary operators.
  case operator {
    BangEqual -> Ok(#(LoxBool(!{ left_value == right_value }), environment2))
    EqualEqual -> Ok(#(LoxBool({ left_value == right_value }), environment2))
    Greater -> {
      case numbers {
        Some(#(left_number, right_number)) ->
          Ok(#(LoxBool(left_number >. right_number), environment2))
        None ->
          Error(RuntimeError(
            message: "binary operator " <> string.inspect(operator) <> " takes two numbers.",
          ))
      }
    }
    GreaterEqual -> {
      case numbers {
        Some(#(left_number, right_number)) ->
          Ok(#(LoxBool(left_number >=. right_number), environment2))
        None ->
          Error(RuntimeError(
            message: "binary operator " <> string.inspect(operator) <> " takes two numbers.",
          ))
      }
    }
    Less -> {
      case numbers {
        Some(#(left_number, right_number)) ->
          Ok(#(LoxBool(left_number <. right_number), environment2))
        None ->
          Error(RuntimeError(
            message: "binary operator " <> string.inspect(operator) <> " takes two numbers.",
          ))
      }
    }
    LessEqual -> {
      case numbers {
        Some(#(left_number, right_number)) ->
          Ok(#(LoxBool(left_number <=. right_number), environment2))
        None ->
          Error(RuntimeError(
            message: "binary operator " <> string.inspect(operator) <> " takes two numbers.",
          ))
      }
    }
    Minus -> {
      case numbers {
        Some(#(left_number, right_number)) ->
          Ok(#(LoxNumber(left_number -. right_number), environment2))
        None ->
          Error(RuntimeError(
            message: "binary operator " <> string.inspect(operator) <> " takes two numbers.",
          ))
      }
    }
    Plus -> {
      case numbers, strings {
        Some(#(left_number, right_number)), None ->
          Ok(#(LoxNumber(left_number +. right_number), environment2))
        None, Some(#(left_string, right_string)) ->
          Ok(#(LoxString(left_string <> right_string), environment2))
        _, _ ->
          Error(RuntimeError(
            message: "binary operator Plus takes either two numbers or two strings.",
          ))
      }
    }

    Slash -> {
      case numbers {
        Some(#(left_number, right_number)) ->
          Ok(#(LoxNumber(left_number /. right_number), environment2))
        None ->
          Error(RuntimeError(
            message: "binary operator " <> string.inspect(operator) <> " takes two numbers.",
          ))
      }
    }
    Star -> {
      case numbers {
        Some(#(left_number, right_number)) ->
          Ok(#(LoxNumber(left_number *. right_number), environment2))
        None ->
          Error(RuntimeError(
            message: "binary operator " <> string.inspect(operator) <> " takes two numbers.",
          ))
      }
    }
    _ ->
      Error(RuntimeError(
        message: "unrecognized token " <> string.inspect(operator) <> " in binary expression.",
      ))
  }
}

fn evaluate_call(
  call_expr: Expr,
  environment: Environment,
) -> LoxResult(#(LoxValue, Environment)) {
  let assert Call(callee, _paren, arguments) = call_expr
  evaluate(callee, environment)
  |> then(fn(result) {
    let #(callee_value, environment1) = result
    let initial_env = case callee_value {
      LoxFunction(closure: closure, ..) -> Ok(#([], closure))
      _ -> Ok(#([], environment1))
    }
    list.fold(arguments, initial_env, evaluate_arguments)
    |> result.map(fn(args_and_environment) {
      let #(args, env) = args_and_environment
      #(callee_value, args, env)
    })
  })
  |> then(call_function)
}

fn evaluate_arguments(
  accumulator: LoxResult(#(List(LoxValue), Environment)),
  argument: Expr,
) -> LoxResult(#(List(LoxValue), Environment)) {
  let assert Ok(#(current_args, current_env)) = accumulator
  let assert Ok(#(argument_value, environment2)) =
    evaluate(argument, current_env)
  Ok(#([argument_value, ..current_args], environment2))
}

fn call_function(
  result: #(LoxValue, List(LoxValue), Environment),
) -> LoxResult(#(LoxValue, Environment)) {
  let #(callee_value, argument_values, environment) = result
  let args_length = list.length(argument_values)
  let message = fn(arity) {
    "Expected " <> string.inspect(arity) <> " arguments but got " <> string.inspect(
      args_length,
    )
  }
  case callee_value {
    NativeFunction(arity: arity, ..) -> {
      case args_length == arity {
        True -> clock(environment)
        False -> Error(RuntimeError(message: message(arity)))
      }
    }
    LoxFunction(arity: arity, declaration: declaration, ..) -> {
      let assert FunDecl(params: params, body: body, ..) = declaration
      case args_length == arity {
        True -> do_call_function(params, argument_values, body, environment)
        False -> Error(RuntimeError(message: message(arity)))
      }
    }
    _ -> Error(RuntimeError(message: "Can only call functions and classes."))
  }
}

fn do_call_function(
  params: List(Token),
  arguments: List(LoxValue),
  body: List(Stmt),
  environment: Environment,
) -> LoxResult(#(LoxValue, Environment)) {
  let fun_environment = environment.create(Some(environment))
  let assert Ok(block_environment) =
    params
    |> list.zip(with: arguments)
    |> list.fold(
      from: fun_environment,
      with: fn(current_env, param_arg_pair) {
        environment.define(
          current_env,
          pair.first(param_arg_pair).value,
          pair.second(param_arg_pair),
        )
      },
    )
    |> block(body, _)

  let return_value = case environment.get(block_environment, ReturnValue) {
    Ok(#(value, _env)) -> value
    Error(_) -> LoxNil
  }
  Ok(#(return_value, environment))
}

fn evaluate_logical(
  operator: TokenType,
  left_expr: Expr,
  right_expr: Expr,
  environment: Environment,
) -> LoxResult(#(LoxValue, Environment)) {
  let assert Ok(#(left_value, environment1)) = evaluate(left_expr, environment)
  case operator, is_truthy(left_value) {
    Or, True | And, False -> Ok(#(left_value, environment1))
    And, True | Or, False -> {
      let assert Ok(#(right_value, environment2)) =
        evaluate(right_expr, environment1)
      Ok(#(right_value, environment2))
    }
  }
}

fn evaluate_unary(
  operator: TokenType,
  right_expr: Expr,
  environment: Environment,
) -> LoxResult(#(LoxValue, Environment)) {
  let assert Ok(#(value, new_environment)) = evaluate(right_expr, environment)
  case operator {
    Bang -> Ok(#(LoxBool(!is_truthy(value)), new_environment))
    Minus -> {
      case value {
        LoxNumber(number) -> Ok(#(LoxNumber(0.0 -. number), new_environment))
        _ ->
          Error(RuntimeError(
            "Unexpected argument to unary '-'. Expected a number, got " <> string.inspect(
              value,
            ) <> ".",
          ))
      }
    }
    _ ->
      Error(RuntimeError(message: "unexpected operator in unary expression."))
  }
}

fn is_truthy(value: LoxValue) -> Bool {
  let is_nil = value == LoxNil
  let is_false = value == LoxBool(False)
  !is_nil && !is_false
}

fn clock(environment: Environment) -> LoxResult(#(LoxValue, Environment)) {
  let seconds = int.to_float(system_time()) /. 1_000_000_000.0
  seconds
  |> string.inspect()
  |> io.println()
  Ok(#(LoxNumber(seconds), environment))
}
