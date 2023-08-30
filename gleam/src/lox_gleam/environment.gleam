//// An environment is a data structure with a reference to its parent
//// environment and a map that records the value assigned to each
//// variable in the current environment.

import gleam/map
import gleam/option
import gleam/result
import gleam/string
import lox_gleam/error
import lox_gleam/types.{Environment, Global, Local, LoxValue, NativeFunction, LoxString, Table, Token}

pub fn create(parent: option.Option(Environment)) -> Environment {
  case parent {
    option.Some(parent_env) -> Local(parent: parent_env, table: map.new())
    option.None ->
      Global(map.new())
      |> add_native_function()
  }
}

pub fn define(
  environment: Environment,
  name: LoxValue,
  value: LoxValue,
) -> Environment {
  case environment {
    Global(table: table) ->
      Global(map.insert(into: table, for: name, insert: value))
    Local(table: table, parent: parent) -> {
      let new_table = map.insert(into: table, for: name, insert: value)
      Local(parent: parent, table: new_table)
    }
  }
}

pub fn define_at_global(
  environment: Environment,
  name: LoxValue,
  value: LoxValue,
) -> Environment {
  case environment {
    Global(..) -> define(environment, name, value)
    Local(parent: Local(..) as parent, table: table) -> {
      let new_parent = define_at_global(parent, name, value)
      Local(parent: new_parent, table: table)
    }
    Local(parent: Global(..) as parent, table: table) -> {
      let new_parent = define(parent, name, value)
      Local(parent: new_parent, table: table)
    }
  }
}

pub fn assign(
  environment: Environment,
  name_token: Token,
  value: LoxValue,
) -> error.LoxResult(Environment) {
  let #(is_global, table) = is_global(environment)
  case is_global, map.has_key(table, name_token.value) {
    True, True ->
      Ok(Global(map.insert(into: table, for: name_token.value, insert: value)))
    False, True -> {
      let new_table =
        map.insert(into: table, for: name_token.value, insert: value)
      let assert Local(parent: parent, ..) = environment
      Ok(Local(parent: parent, table: new_table))
    }
    False, False -> {
      let assert Local(parent: parent, ..) = environment
      assign(parent, name_token, value)
      |> result.then(fn(new_parent) {
        let assert Local(table: table, ..) = environment
        Ok(Local(parent: new_parent, table: table))
      })
    }
    True, False ->
      Error(error.RuntimeError(
        message: "undefined variable '" <> string.inspect(name_token.value) <> "'.",
      ))
  }
}

pub fn get(
  environment: Environment,
  variable: LoxValue,
) -> error.LoxResult(#(LoxValue, Environment)) {
  let #(is_global, table) = is_global(environment)
  case is_global, map.get(table, variable) {
    _, Ok(value) -> Ok(#(value, environment))
    False, Error(Nil) -> {
      let assert Local(parent: parent, ..) = environment
      get(parent, variable)
      |> result.then(fn(value_and_env) {
        // We don't care where the variable was found
        let #(value, _irrelevant_env) = value_and_env
        Ok(#(value, environment))
      })
    }
    True, Error(Nil) ->
      Error(error.RuntimeError(
        message: "undefined variable '" <> string.inspect(variable) <> "'.",
      ))
  }
}

fn is_global(environment) -> #(Bool, Table) {
  case environment {
    Global(table) -> #(True, table)
    Local(table: table, ..) -> #(False, table)
  }
}

fn add_native_function(environment) -> Environment {
  let clock_function =
    NativeFunction(arity: 0, name: "clock", to_string: "<native fn>")
  define(environment, LoxString("clock"), clock_function)
}
