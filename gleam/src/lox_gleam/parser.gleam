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
  Logical, LoxBool, LoxNil, LoxString, Minus, Or, Plus, Print, PrintStmt, Return,
  ReturnStmt, RightBrace, RightParen, Semicolon, Slash, Star, Stmt, Token,
  TokenFalse, TokenNil, TokenNumber, TokenString, TokenType, TrueToken, Unary,
  Var, VarDecl, Variable, While, WhileStmt,
}

type ExprAndTokens =
  #(Expr, List(Token))

type StmtsAndTokens =
  #(List(Stmt), List(Token))

pub fn parse(tokens: LoxResult(List(Token))) -> LoxResult(List(Stmt)) {
  tokens
  |> result.then(fn(result) { declaration(Ok(#([], tokens))) })
  |> result.then(fn(result) {
    case result {
      #(statements, []) | #(statements, [Token(token_type: Eof, ..)]) ->
        list.reverse(statements)
      #(_statements, [Token(..)]) ->
        Error(ParseError(message: "unexpected end of tokens."))
      #(statements, new_tokens) ->
        // This handles the weird case of top-level blocks.
        list.reverse(list.concat([list.reverse(parse(new_tokens)), statements]))
    }
  })
}

fn declaration(
  stmts_and_tokens: LoxResult(StmtsAndTokens),
) -> LoxResult(StmtsAndTokens) {
  case stmts_and_tokens {
    Ok(#(statements, [Token(Eof, ..)])) -> Ok(#(statements, []))
    Ok(#(_statements, [Token(line: line, ..)])) ->
      Error(ParseError(
        message: "you might be missing a semicolon on line " <> string.inspect(
          line,
        ) <> ".",
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

fn basic_statement(statements: List(Stmt), tokens: List(Token), stmt_type) {
  tokens
  |> expression()
  |> then(fn(result) {
    let #(expr, [Token(token_type: Semicolon, ..), ..new_tokens]) = result
    let new_statement = stmt_type(expr)
    declaration(Ok(#([new_statement, ..statements], new_tokens)))
  })
}

fn block(
  existing_statements: List(Stmt),
  tokens: List(Token),
) -> LoxResult(StmtsAndTokens) {
  let assert Ok(first_token) = list.first(tokens)
  case first_token.token_type {
    Eof ->
      Error(ParseError(
        message: "unexpected end of file while parsing block on line " <> int.to_string(
          first_token.line,
        ) <> ".",
      ))
    _ -> {
      declaration(Ok(#([], tokens)))
      |> then(fn(result) {
        let #(block_statements, new_tokens) = result
        let block = Block(list.reverse(block_statements))
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
        message: "line " <> string.inspect(first_token.line) <> ": " <> message,
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
  let [token, ..] = tokens
  case token.token_type {
    Semicolon -> Ok(#(option.None, statements, tokens))
    Var ->
      var_declaration(statements, tokens)
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
  let #(initializer_stmt, statements, tokens) = result

  tokens
  |> condition_expression()
  |> then(fn(condition_and_tokens) {
    let #(condition_expr, new_tokens) = condition_and_tokens
    let for_condition = case condition_expr {
      option.None -> Literal(value: LoxBool(True), line: token.line)
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
      |> then(fn(result) {
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
      |> then(fn(result) {
        let #(expr, [_right_paren, ..new_tokens]) = result
        #(option.Some(expr), new_tokens)
      })
  }
  |> then(fn(increment_and_tokens) {
    let #(increment_expr, other_tokens) = increment_and_tokens
    #(initializer_stmt, for_condition, increment_expr, statements, tokens3)
  })
}

fn for_stmt_body(
  result: #(Option(Stmt), Expr, Option(Expr), List(Stmt), List(Token)),
) -> LoxResult(#(Option(Stmt), Expr, Option(Expr), List(Stmt), List(Token))) {
  let #(initializer_stmt, for_condition, increment_expr, statements, tokens) =
    result
  statement(statements, tokens)
  |> then(fn(stmts_and_tokens) {
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
  fn(result) {
    let #(init_stmt, condition_expr, increment_expr, [body0, ..stmts2], tokens2) =
      result

    let body1 = body_with_increment(increment_expr, body0)

    let while_stmt =
      WhileStmt(condition: condition_expr, body: body1)

    let final_while = case init_stmt {
      option.None -> while_stmt
      option.Some(initializer) -> Block([initializer, while_stmt])
    }
    declaration(Ok(#([final_while, ..stmts2], tokens2)))
  }
}

fn body_with_increment(
  increment_expr: Option(Expr),
  body0: Stmt,
  line: Int,
) -> Stmt {
  case increment_expr {
    option.None -> body0
    option.Some(expr) -> {
      let increment_stmt = ExprStmt(expression: expr, line: line)
      case body0 {
        Block(statements, ..) ->
          Block(list.append(statements, [increment_stmt]), line)
        _ -> Block([body0, increment_stmt], line)
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
      Error(ParseError("Expect '(' after " <> kind <> " name."))
    _, _, _ -> Error(ParseError("Expect " <> kind <> " name."))
  }
  |> then(fn(result) {
    let #(parameters, tokens2) = result
    consume(tokens2, LeftBrace, "Expect '{' after parameter list.")
    |> then(fn(tokens3) {
      build_fun_declaration(parameters, statements, tokens3)
    })
  })
}

fn build_params(
  params: List(Token),
  tokens: List(Token),
) -> LoxResult(#(List(Token), List(Token))) {
  let [param, comma_or_paren, ..new_tokens] = tokens
  case param.token_type, comma_or_paren.token_type {
    Identifier, RightParen -> Ok(#([param, ..params], new_tokens))
    Identifier, Comma -> build_params([param, ..params], new_tokens)
    Identifier, _ -> Error(ParseError("Expect ')' after parameters."))
    _, _ -> Error(ParseError("Expect parameter name."))
  }
}

fn build_fun_declaration(
  parameters: List(Token),
  statements: List(Stmt),
  tokens: List(Token),
) -> LoxResult(StmtsAndTokens) {
  case list.length(parameters) >= 255 {
    True -> Error(ParseError("Can't have more than 255 parameters."))
    False ->
      statements
      |> block(tokens)
      |> then(fn(result1) {
        let assert #([Block(statements: body), ..statements], next_tokens) =
          result1
        let fun_declaration =
          FunDecl(name: name_token, params: parameters, body: body)
        #([fun_declaration, ..statements], next_tokens)
      })
  }
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
    let #(
      [then_branch, ..new_statements],
      [else, ..new_tokens] as result_tokens,
    ) = result
    case else.token_type {
      Else -> if_then_else(condition, then_branch, new_statements, new_tokens)
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
  let #(return_stmt, other_tokens) = case maybe_semicolon.token_type {
    Semicolon -> {
      let nil_return_value = Literal(LoxNil, line: return_token.line)
      let return_stmt =
        ReturnStmt(keyword: return_token, value: nil_return_value)
      #(return_stmt, new_tokens)
    }
    _ ->
      tokens
      |> expression()
      |> then(fn(result) {
        let #(return_value, tokens1) = result
        consume(tokens1, Semicolon, "expect ';' after return statement.")
        |> then(fn(tokens2) {
          let return_stmt = ReturnStmt(value: return_value)
          #(return_stmt, tokens2)
        })
      })
  }
  declaration(Ok(#([return_stmt, ..statements], other_tokens)))
}

fn var_declaration(
  statements: List(Stmt),
  tokens: List(Token),
) -> LoxResult(StmtsAndTokens) {
  consume(tokens, Identifier, "expect variable name.")
  |> then(fn(tokens1) {
    let [maybe_equal, ..tokens2] = tokens1
    case maybe_equal.token_type {
      Equal ->
        var_declaration_with_assignment(variable_name, statements, tokens2)
      Semicolon -> {
        let nil_expr = Literal(LoxNil, variable_name.line)
        let var_declaration =
          VarDecl(name: variable_name, initializer: nil_expr)
        Ok(#([var_declaration, ..statements], tokens2))
      }
      _ ->
        Error(ParseError(
          message: "a variable declaration must be followed by ';' or an assignment.",
        ))
    }
  })
}

fn var_declaration_with_assignment(
  name: Token,
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
    |> then(fn(tokens2) {
      let var_declaration = VarDecl(name: name, initializer: expr)
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
  statement(statements, tokens)
  |> then(fn(result) {
    case result {
      #([body, ..new_statements], new_tokens) -> {
        let while_stmt = WhileStmt(condition: condition, body: body)
        declaration(Ok(#([while_stmt, ..new_statements], new_tokens)))
      }
      _ ->
        Error(ParseError(message: "expected statement as 'while' loop body."))
    }
  })
}

fn expression(tokens: List(Token)) -> LoxResult(ExprAndTokens) {
  assignment(tokens)
}

fn assignment(tokens: List(Token)) -> LoxResult(ExprAndTokens) {
  tokens
  |> or()
  |> then(fn(result) {
    case result {
      #(name_expr, []) ->
        Error(ParseError(
          message: "unexpected end of tokens when assigning variable" <> string.inspect(
            name_expr,
          ) <> ".",
        ))
      #(name_expr, new_tokens) -> do_assignment(name_expr, new_tokens)
    }
  })
}

fn do_assignment(
  name_expr: Expr,
  tokens: List(Token),
) -> LoxResult(ExprAndTokens) {
  let [equals, ..tokens1] = tokens
  case equals.token_type {
    Equal -> do_valid_assignment(name_expr, tokens1)
    _ -> Ok(#(name_expr, tokens))
  }
}

fn do_valid_assignment(name_expr: Expr, tokens: List(Token)) {
  tokens
  |> assignment()
  |> then(fn(result) {
    let #(value_expr, new_tokens) = result
    case name_expr {
      Variable(name: name) ->
        Ok(#(Assign(name: name, value: value_expr), new_tokens))
      _ -> Error(ParseError(message: "invalid assignment target."))
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
      n if n >= 255 ->
        Error(ParseError(message: "Can't have more than 255 arguments."))
      _ -> {
        let call_expr =
          Call(callee: callee, paren: closing_paren, arguments: arguments)
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
      _ -> Error(ParseError(message: "Expect ')' after arguments."))
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
    Identifier -> Ok(#(Variable(name: first_token), other_tokens))
    LeftParen -> do_grouping(first_token, other_tokens)
    Eof -> Ok(#(Literal(value: LoxNil, line: first_token.line), other_tokens))
    _ -> Error(ParseError(message: "unexpected token."))
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
    |> then(fn(tokens2) {
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
  expr_type: fn(TokenType, Expr, Expr, Int) -> Expr,
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
  expr_type: fn(TokenType, Expr, Expr, Int) -> Expr,
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
  expr_type: fn(TokenType, Expr, Expr, Int) -> Expr,
) -> LoxResult(ExprAndTokens) {
  other_tokens
  |> function()
  |> then(fn(result) {
    let #(right, tokens2) = result
    let Token(line: line, token_type: token_type, ..) = first_token
    let binary = expr_type(token_type, left, right, line)
    binary_inner(token_types, function, expr_type)(Ok(#(binary, tokens2)))
  })
}
