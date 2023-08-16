//// An environment is a data structure with a reference to its parent
//// environment and a map that records the value assigned to each
//// variable in the current environment.

import gleam/dynamic
import gleam/map
import gleam/option
import lox_gleam/error
import lox_gleam/token

type Table =
  map.Map(String, dynamic.Dynamic)

pub type Environment {
  Global(table: Table)
  Local(parent: Environment, table: Table)
}

pub fn create(parent: option.Option(Environment)) -> Environment {
  case parent {
    option.Some(parent_env) -> Local(parent: parent_env, table: map.new())
    option.None -> Global(map.new())
  }
}

pub fn define(
  environment: Environment,
  name: String,
  value: dynamic.Dynamic,
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

pub fn assign(
  environment: Environment,
  name_token: token.Token,
  value: dynamic.Dynamic,
) -> error.LoxResult(Environment) {
  let #(is_global, table) = is_global(environment)
  case is_global, map.has_key(table, name_token.lexeme) {
    True, True ->
      Ok(Global(map.insert(into: table, for: name_token.lexeme, insert: value)))
    False, True -> {
      let new_table =
        map.insert(into: table, for: name_token.lexeme, insert: value)
      let assert Local(parent: parent, ..) = environment
      Ok(Local(parent: parent, table: new_table))
    }
    False, False -> {
      let assert Local(parent: parent, ..) = environment
      assign(parent, name_token, value)
    }
    True, False ->
      Error(error.RuntimeError(
        message: "undefined variable '" <> name_token.lexeme <> "'.",
        values: [value],
      ))
  }
}

pub fn get(
  environment: Environment,
  variable: String,
) -> error.LoxResult(#(dynamic.Dynamic, Environment)) {
  let #(is_global, table) = is_global(environment)
  case is_global, map.get(table, variable) {
    _, Ok(value) -> Ok(#(value, environment))
    False, Error(Nil) -> {
      let assert Local(parent: parent, ..) = environment
      get(parent, variable)
    }
    True, Error(Nil) ->
      Error(error.RuntimeError(
        message: "undefined variable '" <> variable <> "'.",
        values: [],
      ))
  }
}

fn is_global(environment) -> #(Bool, Table) {
  case environment {
    Global(table) -> #(True, table)
    Local(table: table, ..) -> #(False, table)
  }
}
