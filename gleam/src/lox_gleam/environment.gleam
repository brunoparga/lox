//// An environment is a map that records the value assigned to each
//// variable.

import gleam/dynamic
import gleam/map
import lox_gleam/error

pub type Environment =
  map.Map(String, dynamic.Dynamic)

pub fn create() -> Environment {
  map.new()
}

pub fn define(
  environment: Environment,
  name: String,
  value: dynamic.Dynamic,
) -> Environment {
  map.insert(into: environment, for: name, insert: value)
}

pub fn get(
  environment: Environment,
  variable: String,
) -> error.LoxResult(dynamic.Dynamic) {
  case map.get(environment, variable) {
    Ok(value) -> Ok(value)
    Error(Nil) ->
      Error(error.RuntimeError(
        message: "undefined variable '" <> variable <> "'.",
        values: [],
      ))
  }
}
