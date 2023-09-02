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
  ReturnValue, Slash, Star, Stmt, TokenType, Unary, VarDecl, Variable, WhileStmt,
}
import lox_gleam/error.{LoxResult, RuntimeError}
import lox_gleam/environment

@external(erlang, "os", "system_time")
fn system_time() -> Int

pub fn interpret(
  statements: LoxResult(List(Stmt)),
  environment: Environment,
) -> LoxResult(Environment) {
  use stmts <- result.try(statements)
  stmts
  |> execute(environment)
  |> result.map(fn(result) {
    let #([], new_environment) = result
    new_environment
  })
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
    [Block(statements: block_statements, ..), ..other_statements] -> {
      block(block_statements, environment)
      |> then(fn(new_environment) { execute(other_statements, new_environment) })
    }
    [ExprStmt(expression: expression, ..), ..other_statements] ->
      expression_stmt(expression, other_statements, environment)
    [FunDecl(..) as statement, ..other_statements] ->
      fun_declaration(statement, other_statements, environment)
    [IfStmt(..) as statement, ..other_statements] ->
      if_stmt(statement, other_statements, environment)
    [PrintStmt(expression: expression, ..), ..other_statements] ->
      print_stmt(expression, other_statements, environment)
    [ReturnStmt(value: return_expr, ..), ..] ->
      return_stmt(return_expr, environment)
    [VarDecl(name: name, initializer: initializer, ..), ..other_statements] ->
      variable_declaration(name, initializer, other_statements, environment)
    [WhileStmt(condition: condition, body: body, ..), ..other_statements] ->
      while_stmt(condition, body, other_statements, environment)
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
  |> result.map(fn(result) {
    let assert #([], Local(parent: new_parent, ..)) = result
    new_parent
  })
  |> result.map_error(fn(_) {
    let [stmt, ..] = block_statements
    RuntimeError("something went wrong with block on line " <> stmt.line)
  })
}

fn fun_declaration(
  fun_decl: Stmt,
  other_statements: List(Stmt),
  environment: Environment,
) -> LoxResult(#(List(Stmt), Environment)) {
  let assert FunDecl(name: name, params: params, ..) = fun_decl
  let function =
    LoxFunction(
      arity: list.length(params),
      to_string: "<fn " <> types.read_value(name) <> ">",
      closure: environment,
      declaration: fun_decl,
    )
  let environment1 = environment.define(environment, name, function)
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
      let assert IfStmt(_line, condition, then_branch, else_branch) =
        if_statement
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
  name: LoxValue,
  initializer: Expr,
  statements: List(Stmt),
  environment: Environment,
) -> LoxResult(#(List(Stmt), Environment)) {
  evaluate(initializer, environment)
  |> then(fn(result) {
    let #(value, environment1) = result
    let environment2 = environment.define(environment1, name, value)
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
    Assign(name: name, value: expr, ..) ->
      evaluate_assignment(name, expr, environment)
    Binary(operator: operator, left: left, right: right, ..) ->
      evaluate_binary(operator, left, right, environment)
    Call(..) -> evaluate_call(expression, environment)
    Grouping(expression: expression, ..) -> evaluate(expression, environment)
    Literal(value: value, ..) -> Ok(#(value, environment))
    Logical(operator: operator, left: left, right: right, ..) ->
      evaluate_logical(operator, left, right, environment)
    Unary(operator: operator, right: right, ..) ->
      evaluate_unary(operator, right, environment)
    Variable(name: name, ..) -> environment.get(environment, name)
    _ -> Error(RuntimeError(message: "unexpected expression found."))
  }
}

fn evaluate_assignment(
  name: LoxValue,
  expression: Expr,
  environment: Environment,
) -> LoxResult(#(LoxValue, Environment)) {
  evaluate(expression, environment)
  |> then(fn(result) { do_assignment(name, result) })
}

fn do_assignment(
  name: LoxValue,
  result: #(LoxValue, Environment),
) -> LoxResult(#(LoxValue, Environment)) {
  let #(value, environment) = result
  environment.assign(environment, name, value)
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
  let assert Call(_line, callee, arguments) = call_expr
  let callee_name = case callee {
    Variable(name: name, ..) -> name
    _ -> LoxString("Unreachable")
  }
  evaluate(callee, environment)
  |> then(fn(result) {
    let #(callee_value, environment1) = result
    list.fold(arguments, Ok(#([], environment1)), evaluate_arguments)
    |> then(fn(args_and_environment) {
      let #(args, env) = args_and_environment
      call_function(callee_name, callee_value, args, env)
    })
  })
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
  callee_name: LoxValue,
  callee_value: LoxValue,
  argument_values: List(LoxValue),
  environment: Environment,
) -> LoxResult(#(LoxValue, Environment)) {
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
    LoxFunction(
      arity: arity,
      declaration: declaration,
      closure: closure,
      to_string: to_string,
    ) -> {
      let assert FunDecl(params: params, body: body, ..) =
        declaration
      case args_length == arity {
        True -> {
          let assert Ok(#(return_value, new_closure, new_environment)) =
            do_call_function(
              params,
              argument_values,
              body,
              closure,
              environment,
            )
          let updated_closure_fun =
            LoxFunction(
              arity: arity,
              declaration: declaration,
              closure: new_closure,
              to_string: to_string,
            )
          let assert Ok(return_environment) =
            environment.assign(
              in: new_environment,
              to: callee_name,
              new_value: updated_closure_fun,
            )
          Ok(#(return_value, return_environment))
        }
        False -> Error(RuntimeError(message: message(arity)))
      }
    }
    _ -> Error(RuntimeError(message: "Can only call functions and classes."))
  }
}

fn do_call_function(
  params: List(LoxValue),
  arguments: List(LoxValue),
  body: List(Stmt),
  closure: Environment,
  environment: Environment,
) -> LoxResult(#(LoxValue, Environment, Environment)) {
  let fun_environment =
    closure
    |> environment.update_parent(environment)
    |> Some()
    |> environment.create()

  let assert Ok(block_environment) =
    params
    |> list.zip(with: arguments)
    |> list.fold(
      from: fun_environment,
      with: fn(current_env, param_arg_pair) {
        environment.define(
          current_env,
          pair.first(param_arg_pair),
          pair.second(param_arg_pair),
        )
      },
    )
    |> block(body, _)

  let #(return_value, tmp_env) = environment.get_return_value(block_environment)
  let assert Local(parent: block_parent, ..) = tmp_env
  let return_environment = environment.update_values(environment, block_parent)
  Ok(#(return_value, block_parent, return_environment))
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
