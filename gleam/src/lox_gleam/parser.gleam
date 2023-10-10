//// Expose `parse`, a function that takes in a list of Lox tokens and
//// returns an abstract syntax tree.

import gleam/list
import gleam/option.{None, Option, Some}
import gleam/result.{then}
import gleam/string
import lox_gleam/error.{LoxResult, ParseError}
import lox_gleam/types.{
  And, Assign, Bang, BangEqual, Binary, Block, Call, Comma, Else, Eof, Equal,
  EqualEqual, Expr, ExprStmt, For, Fun, FunDecl, Greater, GreaterEqual, Grouping,
  Identifier, If, IfStmt, LeftBrace, LeftParen, Less, LessEqual, Literal,
  Logical, LoxBool, LoxNil, LoxString, LoxValue, Minus, Or, Plus, Print,
  PrintStmt, Return, ReturnStmt, RightBrace, RightParen, Semicolon, Slash, Star,
  Stmt, Token, TokenFalse, TokenNil, TokenNumber, TokenString, TokenType,
  TrueToken, Unary, Var, VarDecl, Variable, While, WhileStmt,
}

type ExprAndTokens =
  #(Expr, List(Token))

type StmtsAndTokens =
  #(List(Stmt), List(Token))

pub fn parse(tokens: LoxResult(List(Token))) -> LoxResult(List(Stmt)) {
  tokens
  |> result.then(fn(result) { declaration(Ok(#([], result))) })
  |> result.then(fn(result) {
    case result {
      #(statements, []) | #(statements, [Token(token_type: Eof, ..)]) ->
        Ok(list.reverse(statements))
      #(_statements, [Token(line: line, ..)]) ->
        Error(ParseError(
          message: "unexpected end of tokens.",
          line: line,
          token: LoxString(""),
        ))
      #(statements, new_tokens) -> {
        // This handles the weird case of top-level blocks.
        use new_statements <- then(parse(Ok(new_tokens)))
        let reversed = list.reverse(new_statements)
        [reversed, statements]
        |> list.concat()
        |> list.reverse()
        |> Ok()
      }
    }
  })
}

fn declaration(
  stmts_and_tokens: LoxResult(StmtsAndTokens),
) -> LoxResult(StmtsAndTokens) {
  case stmts_and_tokens {
    Ok(#(statements, [Token(token_type: Eof, ..)])) -> Ok(#(statements, []))
    Ok(#(_statements, [Token(line: line, value: value, ..)])) ->
      Error(ParseError(
        message: "you might be missing a semicolon on line " <> string.inspect(
          line,
        ) <> ".",
        line: line,
        token: value,
      ))
    Ok(#(statements, tokens)) -> statement(statements, tokens)
    Error(error) -> Error(error)
  }
}

fn statement(
  statements: List(Stmt),
  tokens: List(Token),
) -> LoxResult(StmtsAndTokens) {
  let [first_token, ..other_tokens] = tokens
  case first_token.token_type {
    Else -> Ok(#(statements, tokens))
    For -> for_statement(statements, other_tokens)
    Fun ->
      declaration(function_declaration("function", statements, other_tokens))
    If -> if_statement(statements, other_tokens)
    LeftBrace -> block(statements, other_tokens)
    RightBrace -> Ok(#(statements, other_tokens))
    Print -> basic_statement(statements, other_tokens, PrintStmt)
    Return -> return_statement(statements, other_tokens)
    Var -> declaration(var_declaration(statements, other_tokens))
    While -> while_statement(statements, other_tokens)
    _ -> basic_statement(statements, tokens, ExprStmt)
  }
}

fn basic_statement(
  statements: List(Stmt),
  tokens: List(Token),
  stmt_type: fn(String, Expr) -> Stmt,
) {
  tokens
  |> expression()
  |> then(fn(result) {
    let #(expr, [token, ..new_tokens] as all_tokens) =
      result
    let new_statement = stmt_type(token.line, expr)
    // Gambiarra alert! The semicolon needs to be consumed properly, but
    // I am not sure where/how to do that.
    let next_tokens = case token.token_type {
      Semicolon | RightParen -> new_tokens
      _ -> all_tokens
    }
    declaration(Ok(#([new_statement, ..statements], next_tokens)))
  })
}

fn block(
  existing_statements: List(Stmt),
  tokens: List(Token),
) -> LoxResult(StmtsAndTokens) {
  let assert Ok(first_token) = list.first(tokens)
  let line = first_token.line
  case first_token.token_type {
    Eof ->
      Error(ParseError(
        message: "unexpected end of file while parsing block on line " <> line <> ".",
        line: line,
        token: first_token.value,
      ))
    _ -> {
      declaration(Ok(#([], tokens)))
      |> then(fn(result) {
        let #(block_statements, new_tokens) = result
        let block =
          Block(statements: list.reverse(block_statements), line: line)
        Ok(#([block, ..existing_statements], new_tokens))
      })
    }
  }
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

fn for_statement(
  statements: List(Stmt),
  tokens: List(Token),
) -> LoxResult(StmtsAndTokens) {
  tokens
  |> consume(LeftParen, "expect '(' after 'for'.")
  |> then(fn(tokens1) { for_stmt_initializer(statements, tokens1) })
  |> then(for_stmt_condition)
  |> then(for_stmt_increment)
  |> then(for_stmt_body)
  |> then(desugar_into_while)
}

fn for_stmt_initializer(
  statements: List(Stmt),
  tokens: List(Token),
) -> LoxResult(#(Option(Stmt), List(Stmt), List(Token))) {
  let [token, ..other_tokens] = tokens
  case token.token_type {
    Semicolon -> Ok(#(option.None, statements, other_tokens))
    Var ->
      var_declaration(statements, other_tokens)
      |> then(extract_stmt)
    _ -> {
      basic_statement(statements, tokens, ExprStmt)
      |> then(extract_stmt)
    }
  }
}

fn extract_stmt(
  result: #(List(Stmt), List(Token)),
) -> LoxResult(#(Option(Stmt), List(Stmt), List(Token))) {
  let #([stmt, ..other_statements], tokens) = result
  Ok(#(option.Some(stmt), other_statements, tokens))
}

fn for_stmt_condition(
  result: #(Option(Stmt), List(Stmt), List(Token)),
) -> LoxResult(#(Option(Stmt), Expr, List(Stmt), List(Token))) {
  let #(initializer_stmt, statements, [Token(line: line, ..), ..] as tokens) =
    result

  tokens
  |> condition_expression()
  |> result.map(fn(condition_and_tokens) {
    let #(condition_expr, new_tokens) = condition_and_tokens
    let for_condition = case condition_expr {
      option.None -> Literal(value: LoxBool(True), line: line)
      option.Some(expr) -> expr
    }
    #(initializer_stmt, for_condition, statements, new_tokens)
  })
}

fn condition_expression(
  tokens: List(Token),
) -> LoxResult(#(Option(Expr), List(Token))) {
  let [token, ..tokens2] = tokens
  case token.token_type {
    Semicolon -> Ok(#(option.None, tokens2))
    _ ->
      tokens
      |> expression()
      |> result.map(fn(result) {
        let #(expr, token3) = result
        #(option.Some(expr), token3)
      })
  }
}

fn for_stmt_increment(
  result: #(Option(Stmt), Expr, List(Stmt), List(Token)),
) -> LoxResult(#(Option(Stmt), Expr, Option(Expr), List(Stmt), List(Token))) {
  let #(initializer_stmt, for_condition, statements, [token, ..tokens]) = result
  case token.token_type {
    RightParen -> Ok(#(option.None, tokens))
    _ ->
      tokens
      |> expression()
      |> result.map(fn(result) {
        let #(expr, [_right_paren, ..new_tokens]) = result
        #(option.Some(expr), new_tokens)
      })
  }
  |> result.map(fn(increment_and_tokens) {
    let #(increment_expr, other_tokens) = increment_and_tokens
    #(initializer_stmt, for_condition, increment_expr, statements, other_tokens)
  })
}

fn for_stmt_body(
  result: #(Option(Stmt), Expr, Option(Expr), List(Stmt), List(Token)),
) -> LoxResult(#(Option(Stmt), Expr, Option(Expr), List(Stmt), List(Token))) {
  let #(initializer_stmt, for_condition, increment_expr, statements, tokens) =
    result
  statement(statements, tokens)
  |> result.map(fn(stmts_and_tokens) {
    let #(statements_with_body, other_tokens) = stmts_and_tokens
    #(
      initializer_stmt,
      for_condition,
      increment_expr,
      statements_with_body,
      other_tokens,
    )
  })
}

fn desugar_into_while(
  result: #(Option(Stmt), Expr, Option(Expr), List(Stmt), List(Token)),
) -> LoxResult(StmtsAndTokens) {
  let #(init_stmt, condition_expr, increment_expr, [body0, ..stmts2], tokens2) =
    result
  let line = body0.line
  let body1 = body_with_increment(increment_expr, body0, line)
  let while_stmt = WhileStmt(condition: condition_expr, body: body1, line: line)
  let final_while = case init_stmt {
    option.None -> while_stmt
    option.Some(initializer) -> Block([initializer, while_stmt], line: line)
  }
  declaration(Ok(#([final_while, ..stmts2], tokens2)))
}

fn body_with_increment(
  increment_expr: Option(Expr),
  body0: Stmt,
  line: String,
) -> Stmt {
  case increment_expr {
    option.None -> body0
    option.Some(expr) -> {
      let increment_stmt = ExprStmt(expression: expr, line: line)
      case body0 {
        Block(statements: statements, ..) ->
          Block(line, list.append(statements, [increment_stmt]))
        _ -> Block(line, [body0, increment_stmt])
      }
    }
  }
}

fn function_declaration(
  kind: String,
  statements: List(Stmt),
  tokens: List(Token),
) -> LoxResult(StmtsAndTokens) {
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
    consume(tokens2, LeftBrace, "Expect '{' before function body.")
    |> then(fn(tokens3) {
      build_fun_declaration(name_token, parameters, statements, tokens3)
    })
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
  statements: List(Stmt),
  tokens: List(Token),
) -> LoxResult(StmtsAndTokens) {
  statements
  |> block(tokens)
  |> result.map(fn(result1) {
    let assert #(
      [Block(statements: body, line: line), ..statements],
      next_tokens,
    ) = result1
    let fun_declaration =
      FunDecl(
        name: name_token.value,
        params: parameters,
        body: body,
        line: line,
      )
    #([fun_declaration, ..statements], next_tokens)
  })
}

fn if_statement(
  statements: List(Stmt),
  tokens: List(Token),
) -> LoxResult(StmtsAndTokens) {
  consume(tokens, LeftParen, "expect '(' after 'if'.")
  |> then(expression)
  |> then(fn(result) {
    let #(condition, tokens1) = result
    consume(tokens1, RightParen, "expect ')' after if condition.")
    |> then(fn(tokens2) { do_if_statement(condition, statements, tokens2) })
  })
}

fn do_if_statement(
  condition: Expr,
  statements: List(Stmt),
  tokens: List(Token),
) -> LoxResult(StmtsAndTokens) {
  // Currently doesn't work if the then branch is just one statement
  // (like return) outside of a block. It should.
  statements
  |> statement(tokens)
  |> then(fn(result) {
    let #([then_branch, ..new_statements], result_tokens) = result
    case result_tokens {
      [Token(token_type: Else, ..), ..new_tokens] ->
        if_then_else(condition, then_branch, new_statements, new_tokens)
      _ ->
        if_without_else(condition, then_branch, new_statements, result_tokens)
    }
  })
}

fn if_then_else(
  condition: Expr,
  then_branch: Stmt,
  statements: List(Stmt),
  tokens: List(Token),
) -> LoxResult(StmtsAndTokens) {
  statement(statements, tokens)
  |> then(fn(result) {
    let #([else_branch, ..new_statements], new_tokens) = result
    let if_else_stmt =
      IfStmt(
        line: condition.line,
        condition: condition,
        then_branch: then_branch,
        else_branch: option.Some(else_branch),
      )
    declaration(Ok(#([if_else_stmt, ..new_statements], new_tokens)))
  })
}

fn if_without_else(
  condition: Expr,
  then_branch: Stmt,
  statements: List(Stmt),
  tokens: List(Token),
) -> LoxResult(StmtsAndTokens) {
  let if_stmt =
    IfStmt(
      line: condition.line,
      condition: condition,
      then_branch: then_branch,
      else_branch: option.None,
    )
  declaration(Ok(#([if_stmt, ..statements], tokens)))
}

fn return_statement(
  statements: List(Stmt),
  tokens: List(Token),
) -> LoxResult(StmtsAndTokens) {
  let [maybe_semicolon, ..new_tokens] = tokens
  case maybe_semicolon.token_type {
    Semicolon -> {
      let nil_return_value = Literal(LoxNil, line: maybe_semicolon.line)
      let return_stmt =
        ReturnStmt(value: nil_return_value, line: maybe_semicolon.line)
      Ok(#(return_stmt, new_tokens))
    }
    _ ->
      tokens
      |> expression()
      |> result.then(fn(result) {
        let #(return_value, tokens1) = result
        consume(tokens1, Semicolon, "expect ';' after return statement.")
        |> result.then(fn(tokens2) {
          let return_stmt =
            ReturnStmt(value: return_value, line: maybe_semicolon.line)
          Ok(#(return_stmt, tokens2))
        })
      })
  }
  |> result.then(fn(result) {
    let #(return_stmt, other_tokens) = result
    declaration(Ok(#([return_stmt, ..statements], other_tokens)))
  })
}

fn var_declaration(
  statements: List(Stmt),
  tokens: List(Token),
) -> LoxResult(StmtsAndTokens) {
  let [first_token, ..tokens1] = tokens
  case first_token {
    Token(token_type: Identifier, value: variable_name, line: line) -> {
      let [maybe_equal, ..tokens2] = tokens1
      case maybe_equal.token_type {
        Equal ->
          var_declaration_with_assignment(variable_name, statements, tokens2)
        Semicolon -> {
          let nil_expr = Literal(value: LoxNil, line: line)
          let var_declaration =
            VarDecl(name: variable_name, initializer: nil_expr, line: line)
          Ok(#([var_declaration, ..statements], tokens2))
        }
        _ ->
          Error(ParseError(
            message: "the variable declaration on line " <> line <> " must be followed by ';' or an assignment.",
            line: line,
            token: variable_name,
          ))
      }
    }
    Token(line: line, value: value, ..) ->
      Error(ParseError(
        message: "Expect variable name.",
        line: line,
        token: value,
      ))
  }
}

fn var_declaration_with_assignment(
  name: LoxValue,
  statements: List(Stmt),
  tokens: List(Token),
) -> LoxResult(StmtsAndTokens) {
  expression(tokens)
  |> then(fn(result) {
    let #(expr, tokens1) = result
    consume(
      tokens1,
      Semicolon,
      "a variable declaration with assignment must be followed by ';'.",
    )
    |> result.map(fn(tokens2) {
      let var_declaration =
        VarDecl(name: name, initializer: expr, line: expr.line)
      #([var_declaration, ..statements], tokens2)
    })
  })
}

fn while_statement(
  statements: List(Stmt),
  tokens: List(Token),
) -> LoxResult(StmtsAndTokens) {
  consume(tokens, LeftParen, "expect '(' after 'while'.")
  |> then(fn(tokens1) {
    tokens1
    |> expression()
    |> then(fn(result) {
      let #(condition, tokens2) = result
      consume(tokens2, RightParen, "expect ')' after while condition.")
      |> then(fn(tokens3) { do_while_statement(condition, statements, tokens3) })
    })
  })
}

fn do_while_statement(
  condition: Expr,
  statements: List(Stmt),
  tokens: List(Token),
) -> LoxResult(StmtsAndTokens) {
  let [Token(value: value, ..), ..] = tokens
  statement(statements, tokens)
  |> then(fn(result) {
    case result {
      #([body, ..new_statements], new_tokens) -> {
        let while_stmt =
          WhileStmt(condition: condition, body: body, line: condition.line)
        declaration(Ok(#([while_stmt, ..new_statements], new_tokens)))
      }
      _ ->
        Error(ParseError(
          message: "expected statement as 'while' loop body.",
          line: condition.line,
          token: value,
        ))
    }
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
    LeftParen -> do_grouping(first_token, other_tokens)
    Eof -> Ok(#(Literal(value: LoxNil, line: first_token.line), other_tokens))
    _ ->
      Error(ParseError(
        message: "Expect expression.",
        line: first_token.line,
        token: first_token.value,
      ))
  }
}

fn do_grouping(
  first_token: Token,
  tokens: List(Token),
) -> LoxResult(ExprAndTokens) {
  tokens
  |> expression()
  |> then(fn(result) {
    let #(inner_expr, tokens1) = result
    consume(tokens1, RightParen, "unmatched '('.")
    |> result.map(fn(tokens2) {
      let expr = Grouping(expression: inner_expr, line: first_token.line)
      #(expr, tokens2)
    })
  })
}

fn match(token: Token, types: List(TokenType)) -> Bool {
  list.any(types, fn(type_to_match) { token.token_type == type_to_match })
}

/// There was a time when I knew the reason for the existence of this
/// mess of higher-order functions. Oh, ye good olde days.
fn binary_inner(
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

/// It's called recursive descent parsing, and the more recursion the
/// more parser-y it is. I guess this thing has no pars-imony.
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
