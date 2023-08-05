import gleam/erlang
import gleam/erlang/file
import gleam/io
import gleam/string

pub fn main() {
  case erlang.start_arguments() {
    [] -> run_prompt()
    [filename] -> run_file(filename)
    _ -> io.println_error("Usage: gleam run -- [script]")
  }
}

pub fn run_prompt() -> Nil {
  let assert Ok(line) = erlang.get_line("> ")
  run(string.trim(line))
  run_prompt()
}

pub fn run_file(filename: String) -> Nil {
  case file.read(from: filename) {
    Ok(contents) -> run(contents)
    Error(reason) ->
      reason
      |> string.inspect()
      |> io.println_error()
  }
}

pub fn run(code: String) -> Nil {
  io.println("// " <> code)
}
