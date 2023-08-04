import gleam/erlang
import gleam/io
import gleam/string

pub fn main() {
  case erlang.start_arguments() {
    [] -> run_prompt()
    [filename] -> run_file(filename)
    _ -> io.println("Usage: gleam run -- [script]")
  }
}

pub fn run_prompt() -> Nil {
  let assert Ok(line) = erlang.get_line("> ")
  io.println("// " <> string.trim_right(line))
  run_prompt()
}

pub fn run_file(filename) -> Nil {
  io.print("Tried to run the file: ")
  io.println(filename)
}
