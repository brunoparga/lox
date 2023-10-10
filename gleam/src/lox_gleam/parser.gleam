//// Expose `parse`, a function that takes in a list of Lox tokens and
//// returns an abstract syntax tree.

import gleam/list
import gleam/option.{None, Option, Some}
import gleam/result.{then, try}
import gleam/string
import lox_gleam/error.{LoxResult, ParseError}
import lox_gleam/types.{
  And, Assign, Bang, BangEqual, Binary, Block, Call, Comma, Else, Eof, Equal,
  EqualEqual, Expr, ExprStmt, For, Fun, FunDecl, Greater, GreaterEqual, Grouping,
  Identifier, If, IfStmt, LeftBrace, LeftParen, Less, LessEqual, Literal,
  Logical, LoxBool, LoxNil, LoxValue, Minus, Or, Plus, Print,
  PrintStmt, Return, ReturnStmt, RightBrace, RightParen, Semicolon, Slash, Star,
  Stmt, Token, TokenFalse, TokenNil, TokenNumber, TokenString, TokenType,
  TrueToken, Unary, Var, VarDecl, Variable, While, WhileStmt,
}

type ExprAndTokens =
  #(Expr, List(Token))

type StmtsAndTokens =
  #(List(Stmt), List(Token))

type OneStmtAndTokens =
  #(Stmt, List(Token))

pub fn parse(
  stmts_and_tokens: LoxResult(StmtsAndTokens),
) -> LoxResult(List(Stmt)) {
  use #(statements, tokens) <- result.try(stmts_and_tokens)
  case tokens {
    // I'm not sure how to handle the end here.
    [] | [Token(token_type: Eof, ..)] -> Ok(list.reverse(statements))
    _ -> {
      use parsed <- try(declaration(Ok(tokens)))
      let #(new_statement, new_tokens) = parsed
      parse(Ok(#([new_statement, ..statements], new_tokens)))
    }
  }
}

fn declaration(tokens: LoxResult(List(Token))) -> LoxResult(OneStmtAndTokens) {
  use ok_tokens <- result.try(tokens)
  case ok_tokens {
    [Token(token_type: Fun, ..), ..other_tokens] ->
      function_declaration("function", other_tokens)
    [Token(token_type: Var, ..), ..other_tokens] ->
      variable_declaration(other_tokens)
    _ -> statement(tokens)
  }
}

fn function_declaration(
  kind: String,
  tokens: List(Token),
) -> LoxResult(OneStmtAndTokens) {
  let [name_token, left_paren, next_token, ..tokens1] = tokens
  case name_token.token_type, left_paren.token_type, next_token.token_type {
    // No parameters
    Identifier, LeftParen, RightParen -> Ok(#([], tokens1))
    // Yes parameters
    Identifier, LeftParen, _ -> build_params([], [next_token, ..tokens1])
    Identifier, _, _ ->
      Error(ParseError(
        "Expect '(' after " <> kind <> " name.",
        line: name_token.line,
        token: name_token.value,
      ))
    _, _, _ ->
      Error(ParseError(
        "Expect " <> kind <> " name.",
        line: name_token.line,
        token: name_token.value,
      ))
  }
  |> then(fn(result) {
    let #(parameters, tokens2) = result
    let error_message = "Expect '{' before function body."
    use tokens3 <- try(consume(tokens2, LeftBrace, error_message))
    build_fun_declaration(name_token, parameters, tokens3)
  })
}

fn build_params(
  params: List(LoxValue),
  tokens: List(Token),
) -> LoxResult(#(List(LoxValue), List(Token))) {
  let [param, comma_or_paren, ..new_tokens] = tokens
  case list.length(params) >= 255, param.token_type, comma_or_paren.token_type {
    True, _, _ ->
      Error(ParseError(
        "Can't have more than 255 parameters.",
        line: param.line,
        token: param.value,
      ))
    False, Identifier, RightParen ->
      Ok(#(list.reverse([param.value, ..params]), new_tokens))
    False, Identifier, Comma ->
      build_params([param.value, ..params], new_tokens)
    False, Identifier, _ ->
      Error(ParseError(
        "Expect ')' after parameters.",
        line: comma_or_paren.line,
        token: comma_or_paren.value,
      ))
    False, _, _ ->
      Error(ParseError(
        "Expect parameter name.",
        line: param.line,
        token: param.value,
      ))
  }
}

fn build_fun_declaration(
  name_token: Token,
  parameters: List(LoxValue),
  tokens: List(Token),
) -> LoxResult(OneStmtAndTokens) {
  use build_body <- result.try(block(name_token.line, [], tokens))
  let assert #(Block(statements: body, ..), new_tokens) = build_body
  let fun_declaration =
    FunDecl(
      name: name_token.value,
      params: parameters,
      body: body,
      line: name_token.line,
    )
  Ok(#(fun_declaration, new_tokens))
}

fn variable_declaration(tokens: List(Token)) -> LoxResult(OneStmtAndTokens) {
  let [first_token, second_token, ..tokens1] = tokens
  case first_token, second_token.token_type {
    Token(token_type: Identifier, value: variable_name, ..), Equal ->
      var_declaration_with_assignment(variable_name, tokens1)
    Token(token_type: Identifier, value: variable_name, line: line), Semicolon -> {
      let nil_expr = Literal(value: LoxNil, line: line)
      let var_declaration =
        VarDecl(name: variable_name, initializer: nil_expr, line: line)
      Ok(#(var_declaration, tokens1))
    }
    Token(token_type: Identifier, value: value, line: line), _ ->
      Error(ParseError(
        message: "the variable declaration on line " <> line <> " must be followed by ';' or an assignment.",
        line: line,
        token: value,
      ))
    Token(line: line, value: value, ..), _ ->
      Error(ParseError(
        message: "Expect variable name.",
        line: line,
        token: value,
      ))
  }
}

fn var_declaration_with_assignment(
  name: LoxValue,
  tokens: List(Token),
) -> LoxResult(OneStmtAndTokens) {
  use #(expr, tokens1) <- try(expression(tokens))
  let error_message =
    "a variable declaration with assignment must be followed by ';'."
  use tokens2 <- try(consume(tokens1, Semicolon, error_message))
  let var_declaration = VarDecl(name: name, initializer: expr, line: expr.line)
  Ok(#(var_declaration, tokens2))
}

fn statement(tokens: LoxResult(List(Token))) -> LoxResult(OneStmtAndTokens) {
  use ok_tokens <- result.try(tokens)
  case ok_tokens {
    [Token(token_type: For, line: line, ..), ..other_tokens] ->
      for_statement(line, other_tokens)
    [Token(token_type: If, line: line, ..), ..other_tokens] ->
      if_statement(line, other_tokens)
    [Token(token_type: LeftBrace, line: line, ..), ..other_tokens] ->
      block(line, [], other_tokens)
    [Token(token_type: Print, ..), ..other_tokens] ->
      basic_statement(other_tokens, PrintStmt)
    [Token(token_type: Return, ..), ..other_tokens] ->
      return_statement(other_tokens)
    [Token(token_type: While, ..), ..other_tokens] ->
      while_statement(other_tokens)
    _ -> basic_statement(ok_tokens, ExprStmt)
  }
}

fn basic_statement(
  tokens: List(Token),
  stmt_type: fn(String, Expr) -> Stmt,
) -> LoxResult(OneStmtAndTokens) {
  let [Token(line: line, ..), ..] = tokens
  use expr_and_tokens <- result.try(expression(tokens))
  let #(expr, [token, ..new_tokens] as all_tokens) = expr_and_tokens
  let statement = stmt_type(line, expr)
  // Consume the semicolon after the expression, or the right paren
  // if this is a for loop incrementer.
  let next_tokens = case token.token_type {
    Semicolon | RightParen -> new_tokens
    _ -> all_tokens
  }
  Ok(#(statement, next_tokens))
}

fn block(
  line: String,
  block_statements: List(Stmt),
  tokens: List(Token),
) -> LoxResult(OneStmtAndTokens) {
  let [first_token, ..other_tokens] = tokens
  case first_token.token_type {
    Eof ->
      Error(ParseError(
        message: "unexpected end of file while parsing block on line " <> line <> ".",
        line: line,
        token: first_token.value,
      ))
    RightBrace -> {
      let block = Block(statements: list.reverse(block_statements), line: line)
      Ok(#(block, other_tokens))
    }
    _ -> {
      use stmt_and_tokens <- result.try(declaration(Ok(tokens)))
      let #(statement, new_tokens) = stmt_and_tokens
      block(line, [statement, ..block_statements], new_tokens)
    }
  }
}

fn for_statement(
  line: String,
  tokens: List(Token),
) -> LoxResult(OneStmtAndTokens) {
  tokens
  |> consume(LeftParen, "expect '(' after 'for'.")
  |> then(fn(tokens1) { for_stmt_initializer(tokens1) })
  |> then(for_stmt_condition)
  |> then(for_stmt_increment)
  |> then(for_stmt_body)
  |> then(fn(for_stmt) { desugar_into_while(for_stmt, line) })
}

fn for_stmt_initializer(
  tokens: List(Token),
) -> LoxResult(#(Option(Stmt), List(Token))) {
  let [token, ..other_tokens] = tokens
  case token.token_type {
    Semicolon -> Ok(#(option.None, other_tokens))
    Var ->
      variable_declaration(other_tokens)
      |> then(wrap_in_option)
    _ ->
      basic_statement(tokens, ExprStmt)
      |> then(wrap_in_option)
  }
}

fn wrap_in_option(
  previous_result: OneStmtAndTokens,
) -> LoxResult(#(Option(Stmt), List(Token))) {
  let #(statement, tokens) = previous_result
  Ok(#(option.Some(statement), tokens))
}

fn for_stmt_condition(
  previous_result: #(Option(Stmt), List(Token)),
) -> LoxResult(#(Option(Stmt), Expr, List(Token))) {
  let #(
    initializer_stmt,
    [Token(line: line, token_type: token_type, ..), ..other_tokens] as tokens,
  ) = previous_result

  case token_type {
    Semicolon -> Ok(#(Literal(value: LoxBool(True), line: line), other_tokens))
    _ -> {
      use #(condition, tokens1) <- try(expression(tokens))
      consume(tokens1, Semicolon, "Expect ';' after `for` condition.")
      |> then(fn(tokens2) { Ok(#(condition, tokens2)) })
    }
  }
  |> result.then(fn(condition_and_tokens) {
    let #(for_condition, new_tokens) = condition_and_tokens
    Ok(#(initializer_stmt, for_condition, new_tokens))
  })
}

fn for_stmt_increment(
  previous_result: #(Option(Stmt), Expr, List(Token)),
) -> LoxResult(#(Option(Stmt), Expr, Option(Expr), List(Token))) {
  let #(initializer_stmt, for_condition, [token, ..other_tokens] as tokens) =
    previous_result
  case token.token_type {
    RightParen -> Ok(#(option.None, other_tokens))
    _ ->
      tokens
      |> expression()
      |> result.then(fn(result) {
        let #(expr, tokens1) = result
        use tokens2 <- try(consume(
          tokens1,
          RightParen,
          "Expect ')' after `for` incrementer.",
        ))
        Ok(#(option.Some(expr), tokens2))
      })
  }
  |> result.map(fn(increment_and_tokens) {
    let #(increment_expr, other_tokens) = increment_and_tokens
    #(initializer_stmt, for_condition, increment_expr, other_tokens)
  })
}

fn for_stmt_body(
  previous_result: #(Option(Stmt), Expr, Option(Expr), List(Token)),
) -> LoxResult(#(Option(Stmt), Expr, Option(Expr), Stmt, List(Token))) {
  let #(initializer_stmt, for_condition, increment_expr, tokens) =
    previous_result
  statement(Ok(tokens))
  |> result.map(fn(stmt_and_tokens) {
    let #(body, other_tokens) = stmt_and_tokens
    #(initializer_stmt, for_condition, increment_expr, body, other_tokens)
  })
}

fn desugar_into_while(
  result: #(Option(Stmt), Expr, Option(Expr), Stmt, List(Token)),
  line: String,
) -> LoxResult(OneStmtAndTokens) {
  let #(init_stmt, condition_expr, increment_expr, body, tokens) = result
  let body1 = body_with_increment(increment_expr, body)
  let while_stmt = WhileStmt(condition: condition_expr, body: body1, line: line)
  let final_while = case init_stmt {
    option.None -> while_stmt
    option.Some(initializer) -> Block([initializer, while_stmt], line: line)
  }
  Ok(#(final_while, tokens))
}

fn body_with_increment(increment_expr: Option(Expr), body: Stmt) -> Stmt {
  case increment_expr {
    option.None -> body
    option.Some(expr) -> {
      let increment_stmt = ExprStmt(expression: expr, line: expr.line)
      case body {
        Block(statements: statements, line: line) ->
          Block(line, list.append(statements, [increment_stmt]))
        _ -> Block(body.line, [body, increment_stmt])
      }
    }
  }
}

fn if_statement(
  line: String,
  tokens: List(Token),
) -> LoxResult(OneStmtAndTokens) {
  consume(tokens, LeftParen, "expect '(' after 'if'.")
  |> then(expression)
  |> then(fn(condition_and_tokens) {
    let #(condition, tokens1) = condition_and_tokens
    consume(tokens1, RightParen, "expect ')' after if condition.")
    |> then(fn(tokens2) { do_if_statement(line, condition, tokens2) })
  })
}

fn do_if_statement(
  line: String,
  condition: Expr,
  tokens: List(Token),
) -> LoxResult(OneStmtAndTokens) {
  use #(then_branch, result_tokens) <- try(statement(Ok(tokens)))
  case result_tokens {
    [Token(token_type: Else, ..), ..new_tokens] ->
      if_then_else(line, condition, then_branch, new_tokens)
    _ -> {
      let if_stmt =
        IfStmt(
          line: line,
          condition: condition,
          then_branch: then_branch,
          else_branch: option.None,
        )
      Ok(#(if_stmt, tokens))
    }
  }
}

fn if_then_else(
  line: String,
  condition: Expr,
  then_branch: Stmt,
  tokens: List(Token),
) -> LoxResult(OneStmtAndTokens) {
  use #(else_branch, new_tokens) <- try(statement(Ok(tokens)))
  let if_else_stmt =
    IfStmt(
      line: line,
      condition: condition,
      then_branch: then_branch,
      else_branch: option.Some(else_branch),
    )
  Ok(#(if_else_stmt, new_tokens))
}

fn return_statement(tokens: List(Token)) -> LoxResult(OneStmtAndTokens) {
  let [maybe_semicolon, ..new_tokens] = tokens
  case maybe_semicolon.token_type {
    Semicolon -> {
      let nil_return_value = Literal(LoxNil, line: maybe_semicolon.line)
      let return_stmt =
        ReturnStmt(value: nil_return_value, line: maybe_semicolon.line)
      Ok(#(return_stmt, new_tokens))
    }
    _ -> {
      use #(return_value, tokens1) <- try(expression(tokens))
      let error_message = "expect ';' after return statement."
      use tokens2 <- try(consume(tokens1, Semicolon, error_message))
      let return_stmt =
        ReturnStmt(value: return_value, line: maybe_semicolon.line)
      Ok(#(return_stmt, tokens2))
    }
  }
}

fn while_statement(tokens: List(Token)) -> LoxResult(OneStmtAndTokens) {
  use tokens1 <- try(consume(tokens, LeftParen, "expect '(' after 'while'."))
  use #(condition, tokens2) <- try(expression(tokens1))
  use tokens3 <- try(consume(
    tokens2,
    RightParen,
    "expect ')' after while condition.",
  ))
  do_while_statement(condition, tokens3)
}

fn do_while_statement(
  condition: Expr,
  tokens: List(Token),
) -> LoxResult(OneStmtAndTokens) {
  let [Token(value: value, ..), ..] = tokens
  statement(Ok(tokens))
  |> result.replace_error(ParseError(
    message: "expected statement as 'while' loop body.",
    line: condition.line,
    token: value,
  ))
  |> result.then(fn(body_and_tokens) {
    let #(body, new_tokens) = body_and_tokens
    let while_stmt =
      WhileStmt(condition: condition, body: body, line: condition.line)
    Ok(#(while_stmt, new_tokens))
  })
}

fn expression(tokens: List(Token)) -> LoxResult(ExprAndTokens) {
  assignment(tokens)
}

fn assignment(tokens: List(Token)) -> LoxResult(ExprAndTokens) {
  let [Token(value: value, ..), ..] = tokens
  tokens
  |> or()
  |> then(fn(result) {
    case result {
      #(name_expr, []) ->
        Error(ParseError(
          message: "unexpected end of tokens when assigning variable" <> string.inspect(
            name_expr,
          ) <> ".",
          line: name_expr.line,
          token: value,
        ))
      #(name_expr, new_tokens) -> do_assignment(name_expr, new_tokens)
    }
  })
}

fn do_assignment(
  name_expr: Expr,
  tokens: List(Token),
) -> LoxResult(ExprAndTokens) {
  let [equals, ..] = tokens
  case equals.token_type {
    Equal -> do_valid_assignment(name_expr, tokens)
    _ -> Ok(#(name_expr, tokens))
  }
}

fn do_valid_assignment(name_expr: Expr, tokens: List(Token)) {
  let [Token(value: value, ..), ..tokens1] = tokens
  tokens1
  |> assignment()
  |> then(fn(result) {
    let #(value_expr, new_tokens) = result
    case name_expr {
      Variable(name: name, line: line) ->
        Ok(#(Assign(name: name, value: value_expr, line: line), new_tokens))
      _ ->
        Error(ParseError(
          message: "Invalid assignment target.",
          line: value_expr.line,
          token: value,
        ))
    }
  })
}

fn or(tokens: List(Token)) -> LoxResult(ExprAndTokens) {
  tokens
  |> and()
  |> binary_inner([Or], and, Logical)
}

fn and(tokens: List(Token)) -> LoxResult(ExprAndTokens) {
  tokens
  |> equality()
  |> binary_inner([And], equality, Logical)
}

fn equality(tokens: List(Token)) -> LoxResult(ExprAndTokens) {
  tokens
  |> comparison()
  |> binary_inner([BangEqual, EqualEqual], comparison, Binary)
}

fn comparison(tokens: List(Token)) -> LoxResult(ExprAndTokens) {
  tokens
  |> term()
  |> binary_inner([Greater, GreaterEqual, Less, LessEqual], term, Binary)
}

fn term(tokens: List(Token)) -> LoxResult(ExprAndTokens) {
  tokens
  |> factor()
  |> binary_inner([Minus, Plus], factor, Binary)
}

fn factor(tokens: List(Token)) -> LoxResult(ExprAndTokens) {
  tokens
  |> unary()
  |> binary_inner([Slash, Star], unary, Binary)
}

fn unary(tokens: List(Token)) -> LoxResult(ExprAndTokens) {
  let [first_token, ..tokens1] = tokens
  case match(first_token, [Bang, Minus]) {
    True -> do_unary(first_token, tokens1)
    False -> call(tokens)
  }
}

fn do_unary(first_token: Token, tokens: List(Token)) -> LoxResult(ExprAndTokens) {
  tokens
  |> unary()
  |> then(fn(result) {
    let #(right, new_tokens) = result
    let Token(line: line, token_type: token_type, ..) = first_token
    let unary_expr = Unary(operator: token_type, right: right, line: line)
    Ok(#(unary_expr, new_tokens))
  })
}

fn call(tokens: List(Token)) -> LoxResult(ExprAndTokens) {
  tokens
  |> primary()
  |> then(fn(result) {
    // `primary` might return, for example, a literal (e.g. `true`).
    // In that case, it should not have a left paren after it; so we
    // just return it up the recursive expression evaluation chain.
    // If it has a left paren, it must be an identifier for something
    // that is callable, so we run `finish_call`.
    let #(callee, [left_paren, ..remaining_tokens] as new_tokens) = result
    case left_paren.token_type {
      LeftParen -> finish_call(callee, remaining_tokens)
      _ -> Ok(#(callee, new_tokens))
    }
  })
}

fn finish_call(callee: Expr, tokens: List(Token)) -> LoxResult(ExprAndTokens) {
  let [right_paren, ..other_tokens] = tokens
  case right_paren.token_type {
    RightParen -> Ok(#([], right_paren, other_tokens))
    _ -> build_arguments([], tokens)
  }
  |> then(fn(result) {
    let #(arguments, closing_paren, [left_paren, ..new_tokens]) = result
    case list.length(arguments) {
      n if n >= 255 -> {
        // for error reporting, we need the token *before* the closing paren.
        let assert Ok(error_token) =
          tokens
          |> list.take_while(fn(token) { token != closing_paren })
          |> list.last()
        Error(ParseError(
          message: "Can't have more than 255 arguments.",
          line: error_token.line,
          token: error_token.value,
        ))
      }
      _ -> {
        let call_expr =
          Call(line: callee.line, callee: callee, arguments: arguments)
        case left_paren.token_type {
          // Multiple (chained) calls like foo()()()
          LeftParen -> finish_call(call_expr, new_tokens)
          // `left_paren` here is actually a semicolon, needed for matching
          // at `basic_statement`
          _ -> Ok(#(call_expr, [left_paren, ..new_tokens]))
        }
      }
    }
  })
}

fn build_arguments(
  arguments: List(Expr),
  tokens: List(Token),
) -> LoxResult(#(List(Expr), Token, List(Token))) {
  tokens
  |> expression()
  |> then(fn(result) {
    let #(argument, [comma_or_paren, ..new_tokens]) = result
    case comma_or_paren.token_type {
      RightParen -> Ok(#([argument, ..arguments], comma_or_paren, new_tokens))
      Comma -> build_arguments([argument, ..arguments], new_tokens)
      _ ->
        Error(ParseError(
          message: "Expect ')' after arguments.",
          line: comma_or_paren.line,
          token: comma_or_paren.value,
        ))
    }
  })
}

fn primary(tokens: List(Token)) -> LoxResult(ExprAndTokens) {
  let [first_token, ..other_tokens] = tokens
  case first_token.token_type {
    TokenFalse ->
      Ok(#(Literal(value: LoxBool(False), line: first_token.line), other_tokens))
    TrueToken ->
      Ok(#(Literal(value: LoxBool(True), line: first_token.line), other_tokens))
    TokenNil ->
      Ok(#(Literal(value: LoxNil, line: first_token.line), other_tokens))
    TokenString | TokenNumber ->
      Ok(#(
        Literal(value: first_token.value, line: first_token.line),
        other_tokens,
      ))
    Identifier ->
      Ok(#(
        Variable(line: first_token.line, name: first_token.value),
        other_tokens,
      ))
    LeftParen -> do_grouping(first_token.line, other_tokens)
    Eof -> Ok(#(Literal(value: LoxNil, line: first_token.line), other_tokens))
    _ ->
      Error(ParseError(
        message: "Expect expression.",
        line: first_token.line,
        token: first_token.value,
      ))
  }
}

fn do_grouping(line: String, tokens: List(Token)) -> LoxResult(ExprAndTokens) {
  tokens
  |> expression()
  |> then(fn(result) {
    let #(inner_expr, tokens1) = result
    consume(tokens1, RightParen, "unmatched '('.")
    |> result.map(fn(tokens2) {
      let expr = Grouping(expression: inner_expr, line: line)
      #(expr, tokens2)
    })
  })
}

fn match(token: Token, types: List(TokenType)) -> Bool {
  list.any(types, fn(type_to_match) { token.token_type == type_to_match })
}

fn consume(
  tokens: List(Token),
  token_type: TokenType,
  message: String,
) -> LoxResult(List(Token)) {
  let [first_token, ..tokens1] = tokens
  case first_token.token_type == token_type {
    True -> Ok(tokens1)
    False ->
      Error(ParseError(
        message: message,
        line: first_token.line,
        token: first_token.value,
      ))
  }
}

// LASCIATE OGNI SPERANZA, VOI CH'ENTRATE

fn binary_inner(
  // As an example, `factor` (the highest-precedence binary expression) calls
  // this thing with ([Slash, Star], unary, Binary) as arguments.
  token_types: List(TokenType),
  function: fn(List(Token)) -> LoxResult(ExprAndTokens),
  expr_type: fn(String, TokenType, Expr, Expr) -> Expr,
) -> fn(LoxResult(ExprAndTokens)) -> LoxResult(ExprAndTokens) {
  fn(expr_and_tokens) -> LoxResult(ExprAndTokens) {
    expr_and_tokens
    |> then(fn(result) {
      let #(expr, tokens) = result
      happy_path(token_types, function, expr_type)(expr, tokens)
    })
  }
}

//  The return value is:
//  fn(expr_and_tokens) -> LoxResult(ExprAndTokens) {
//    expr_and_tokens
//    |> then(fn(result) {
//      let #(expr, tokens) = result
//      happy_path([Slash, Star], unary, Binary)(expr, tokens)
//  })
//  }

//  `happy_path` is called with the same arguments as `binary_inner`.
//  It expands to:
//  fn(expr, tokens) -> LoxResult(ExprAndTokens) {
//    let [first_token, ..other_tokens] = tokens
//    case match(first_token, [Slash, Star]) {
//      True ->
//        build_binary(
//          [Slash, Star],
//          unary,
//          expr,
//          first_token,
//          other_tokens,
//          Binary,
//        )
//      False -> Ok(#(expr, tokens))
//    }
//  }

//  Now one has to expand that call to build_binary.
//  It goes like:
//  other_tokens
//  |> unary
//  |> then(fn(result) {
//    let #(right, tokens2) = result
//    let Token(line: line, token_type: token_type, ..) = first_token
//    let binary = Binary(line, token_type, left, right)
//    binary_inner([Slash, Star], unary, Binary)(Ok(THIS_BINARY, tokens2))
//  })

//  So this returns whatever the function returned by binary_inner does,
//  when called with the binary expression and the new tokens. (Why don't
//  I just return the binary and the tokens??)

//  fn(THIS_BINARY_and_tokens2) -> LoxResult(ExprAndTokens) {
//    THIS_BINARY_and_tokens2
//    |> then(fn(result) {
//      let #(THIS_BINARY, tokens2) = result
//      happy_path([Slash, Star], unary, Binary)(THIS_BINARY, tokens2)
//    })
//  }

//  We then evaluate the function that happy_path returns...
//  fn(THIS_BINARY, tokens2) -> LoxResult(ExprAndTokens) {
//    let [first_token, ..other_tokens] = tokens2
//    case match(first_token, [Slash, Star]) {
//      True ->
//        build_binary(
//          [Slash, Star],
//          unary,
//          THIS_BINARY,
//          first_token,
//          other_tokens,
//          Binary,
//        )
//      False -> Ok(#(THIS_BINARY, tokens))
//    }
//  }

//  Presumably, `first_token` here is not a Slash or Star, so the recursion
//  ends by returning THIS_BINARY

fn happy_path(
  token_types: List(TokenType),
  function: fn(List(Token)) -> LoxResult(ExprAndTokens),
  expr_type: fn(String, TokenType, Expr, Expr) -> Expr,
) -> fn(Expr, List(Token)) -> LoxResult(ExprAndTokens) {
  fn(expr, tokens) {
    let [first_token, ..other_tokens] = tokens
    case match(first_token, token_types) {
      True ->
        build_binary(
          token_types,
          function,
          expr,
          first_token,
          other_tokens,
          expr_type,
        )
      False -> Ok(#(expr, tokens))
    }
  }
}

fn build_binary(
  token_types: List(TokenType),
  function: fn(List(Token)) -> LoxResult(ExprAndTokens),
  left: Expr,
  first_token: Token,
  other_tokens: List(Token),
  expr_type: fn(String, TokenType, Expr, Expr) -> Expr,
) -> LoxResult(ExprAndTokens) {
  other_tokens
  |> function()
  |> then(fn(result) {
    let #(right, tokens2) = result
    let Token(line: line, token_type: token_type, ..) = first_token
    let binary = expr_type(line, token_type, left, right)
    binary_inner(token_types, function, expr_type)(Ok(#(binary, tokens2)))
  })
}
