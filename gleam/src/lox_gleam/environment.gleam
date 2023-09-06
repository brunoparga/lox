//// An environment is a data structure with a reference to its parent
//// environment and a map that records the value assigned to each
//// variable in the current environment.

import gleam/map
import gleam/option
import gleam/result
import lox_gleam/error
import lox_gleam/types.{
  Environment, Global, Local, LoxNil, LoxString, LoxValue, NativeFunction,
  ReturnValue, Table,
}

pub fn assign(
  at line: String,
  in environment: Environment,
  to name: LoxValue,
  new_value value: LoxValue,
) -> error.LoxResult(Environment) {
  let #(is_global, table) = is_global(environment)
  case is_global, map.has_key(table, types.read_value(name)) {
    True, True ->
      Ok(Global(map.insert(
        into: table,
        for: types.read_value(name),
        insert: value,
      )))
    False, True -> {
      let new_table =
        map.insert(into: table, for: types.read_value(name), insert: value)
      let assert Local(parent: parent, ..) = environment
      Ok(Local(parent: parent, table: new_table))
    }
    False, False -> {
      let assert Local(parent: parent, ..) = environment
      assign(line, parent, name, value)
      |> result.then(fn(new_parent) {
        let assert Local(table: table, ..) = environment
        Ok(Local(parent: new_parent, table: table))
      })
    }
    True, False ->
      Error(error.RuntimeError(
        message: "undefined variable '" <> types.read_value(name) <> "'.",
        line: line,
      ))
  }
}

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
      Global(map.insert(into: table, for: types.read_value(name), insert: value))
    Local(table: table, parent: parent) -> {
      let new_table =
        map.insert(into: table, for: types.read_value(name), insert: value)
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

pub fn get(
  line: String,
  environment: Environment,
  variable: LoxValue,
) -> error.LoxResult(#(LoxValue, Environment)) {
  let #(is_global, table) = is_global(environment)
  case is_global, map.get(table, types.read_value(variable)) {
    _, Ok(value) -> Ok(#(value, environment))
    False, Error(Nil) -> {
      let assert Local(parent: parent, ..) = environment
      get(line, parent, variable)
      |> result.then(fn(value_and_env) {
        // We don't care where the variable was found
        let #(value, _irrelevant_env) = value_and_env
        Ok(#(value, environment))
      })
    }
    True, Error(Nil) ->
      Error(error.RuntimeError(
        message: "undefined variable '" <> types.read_value(variable) <> "'.",
        line: line,
      ))
  }
}

pub fn get_return_value(
  line: String,
  environment: Environment,
) -> #(LoxValue, Environment) {
  let return_value = case get(line, environment, ReturnValue) {
    Ok(#(value, _env)) -> value
    Error(_) -> LoxNil
  }
  let new_env = delete_return_value(environment)
  #(return_value, new_env)
}

pub fn update_parent(closure: Environment, parent: Environment) -> Environment {
  case closure {
    Global(..) -> update_values(closure, parent)
    Local(table: closure_table, parent: closure_parent) -> {
      let new_parent = update_values(closure_parent, parent)
      Local(table: closure_table, parent: new_parent)
    }
  }
}

pub fn update_values(target: Environment, source: Environment) -> Environment {
  let new_table = map.merge(target.table, source.table)
  case target {
    Global(..) -> Global(table: new_table)
    Local(parent: parent, ..) -> Local(parent: parent, table: new_table)
  }
}

fn delete_return_value(environment: Environment) -> Environment {
  case environment {
    Global(table) -> Global(map.delete(table, "ReturnValue"))
    Local(parent: parent, table: table) ->
      Local(parent: delete_return_value(parent), table: table)
  }
}

fn add_native_function(environment: Environment) -> Environment {
  let clock_function =
    NativeFunction(arity: 0, name: "clock", to_string: "<native fn>")
  define(environment, LoxString("clock"), clock_function)
}

fn is_global(environment: Environment) -> #(Bool, Table) {
  case environment {
    Global(table) -> #(True, table)
    Local(table: table, ..) -> #(False, table)
  }
}
