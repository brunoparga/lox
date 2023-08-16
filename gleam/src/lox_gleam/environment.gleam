//// An environment is a map that records the value assigned to each
//// variable.

import gleam/dynamic
import gleam/map
import lox_gleam/error
import lox_gleam/token

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

pub fn assign(
  environment: Environment,
  name_token: token.Token,
  value: dynamic.Dynamic,
) {
  case map.has_key(environment, name_token.lexeme) {
    True ->
      Ok(map.insert(into: environment, for: name_token.lexeme, insert: value))
    False ->
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
  case map.get(environment, variable) {
    Ok(value) -> Ok(#(value, environment))
    Error(Nil) ->
      Error(error.RuntimeError(
        message: "undefined variable '" <> variable <> "'.",
        values: [],
      ))
  }
}
