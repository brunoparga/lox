//// Expose `parse`, a function that takes in a list of Lox tokens and
//// returns an abstract syntax tree.

import gleam/list
import gleam/option
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
    Ok(#(_statements, [one_token])) ->
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
    Print -> do_statement(statements, other_tokens, PrintStmt)
    Return -> return_statement(first_token, statements, other_tokens)
    Var -> declaration(var_declaration(statements, other_tokens))
    While -> while_statement(statements, other_tokens)
    _ -> do_statement(statements, tokens, ExprStmt)
  }
}

fn for_statement(
  statements: List(Stmt),
  tokens: List(Token),
) -> LoxResult(StmtsAndTokens) {
  let stmt_extractor = fn(result) {
    let #([stmt, ..new_statements], new_tokens) = result
    Ok(#(option.Some(stmt), new_statements, new_tokens))
  }
  let [first_token, ..tokens1] = tokens
  case first_token.token_type {
    LeftParen -> {
      let [second_token, ..tokens2] = tokens1
      let assert Ok(#(initializer_stmt, statements2, tokens3)) = case
        second_token.token_type
      {
        Semicolon -> Ok(#(option.None, statements, tokens2))
        Var ->
          var_declaration(statements, tokens2)
          |> then(stmt_extractor)
        _ -> {
          do_statement(statements, tokens2, ExprStmt)
          |> then(stmt_extractor)
        }
      }

      let [third_token, ..tokens4] = tokens3
      let assert Ok(#(condition_expr, tokens5)) = case third_token.token_type {
        Semicolon -> Ok(#(option.None, tokens4))
        _ ->
          tokens3
          |> expression()
          |> then(fn(result) {
            let #(expr, new_tokens) = result
            Ok(#(option.Some(expr), new_tokens))
          })
      }

      let [fourth_token, ..tokens6] = tokens5
      let assert Ok(#(increment_expr, tokens7)) = case fourth_token.token_type {
        RightParen -> Ok(#(option.None, tokens6))
        _ ->
          tokens6
          |> expression()
          |> then(fn(result) {
            let #(expr, [_right_paren, ..new_tokens]) = result
            Ok(#(option.Some(expr), new_tokens))
          })
      }

      let assert Ok(#([body0, ..statements3], tokens8)) =
        statement(statements2, tokens7)

      let body1 = case increment_expr {
        option.None -> body0
        option.Some(expr) -> {
          let increment_stmt = ExprStmt(expr)
          case body0 {
            Block(statements) ->
              Block(list.append(statements, [increment_stmt]))
            _ -> Block([body0, increment_stmt])
          }
        }
      }

      let while_condition = case condition_expr {
        option.None -> Literal(value: LoxBool(True), line: third_token.line)
        option.Some(expr) -> expr
      }

      let while_stmt = WhileStmt(condition: while_condition, body: body1)

      let final_while = case initializer_stmt {
        option.None -> while_stmt
        option.Some(init_stmt) -> Block([init_stmt, while_stmt])
      }
      declaration(Ok(#([final_while, ..statements3], tokens8)))
    }
    _ -> Error(ParseError(message: "expect '(' after 'for'."))
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
    let #(parameters, [left_brace, ..tokens2]) = result
    case list.length(parameters) >= 255, left_brace.token_type {
      True, _ -> Error(ParseError("Can't have more than 255 parameters."))
      False, LeftBrace ->
        block(statements, tokens2)
        |> then(fn(result1) {
          let assert #([Block(statements: body), ..statements], tokens3) =
            result1
          let fun_declaration =
            FunDecl(name: name_token, params: parameters, body: body)
          Ok(#([fun_declaration, ..statements], tokens3))
        })
      False, _ -> Error(ParseError("Expect '{' after parameter list."))
    }
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

fn if_statement(
  statements: List(Stmt),
  tokens: List(Token),
) -> LoxResult(StmtsAndTokens) {
  let [first_token, ..new_tokens] = tokens
  case first_token.token_type {
    LeftParen -> if_condition_is_ok(statements, new_tokens)
    _ -> Error(ParseError(message: "expect '(' after 'if'."))
  }
}

fn if_condition_is_ok(
  statements: List(Stmt),
  tokens: List(Token),
) -> LoxResult(StmtsAndTokens) {
  tokens
  |> expression()
  |> then(fn(result) {
    let #(condition, [right_paren, ..new_tokens]) = result
    case right_paren.token_type {
      RightParen -> do_if_statement(condition, statements, new_tokens)
      _ -> Error(ParseError(message: "expect ')' after if condition."))
    }
  })
}

fn do_if_statement(
  condition: Expr,
  statements: List(Stmt),
  tokens: List(Token),
) -> LoxResult(StmtsAndTokens) {
  // Currently doesn't work if the then branch is just one statement
  // (like return) outside of a block. It should.
  statement(statements, tokens)
  |> then(fn(result) {
    case result {
      #([then_branch, ..new_statements], [else, ..new_tokens] as result_tokens) -> {
        case else.token_type {
          Else ->
            if_then_else(condition, then_branch, new_statements, new_tokens)
          _ ->
            if_without_else(
              condition,
              then_branch,
              new_statements,
              result_tokens,
            )
        }
      }
      #(_, [not_else, ..]) ->
        Error(ParseError(
          message: "Error at '" <> string.inspect(not_else.value) <> "': Expect expression.",
        ))
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
  return_token: Token,
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
    _ -> {
      let assert Ok(#(
        return_value,
        [Token(token_type: Semicolon, ..), ..tokens1],
      )) = expression(tokens)
      let return_stmt = ReturnStmt(keyword: return_token, value: return_value)
      #(return_stmt, tokens1)
    }
  }
  declaration(Ok(#([return_stmt, ..statements], other_tokens)))
}

fn var_declaration(
  statements: List(Stmt),
  tokens: List(Token),
) -> LoxResult(StmtsAndTokens) {
  let [variable_name, ..tokens1] = tokens
  case variable_name.token_type {
    Identifier -> do_var_declaration(variable_name, statements, tokens1)
    _ -> Error(ParseError(message: "expect variable name."))
  }
}

fn do_var_declaration(
  variable_name: Token,
  statements: List(Stmt),
  tokens: List(Token),
) -> LoxResult(StmtsAndTokens) {
  let [maybe_equal, ..new_tokens] = tokens
  case maybe_equal.token_type {
    Equal ->
      var_declaration_with_assignment(variable_name, statements, new_tokens)
    Semicolon -> {
      let nil_expr = Literal(LoxNil, variable_name.line)
      let var_declaration = VarDecl(name: variable_name, initializer: nil_expr)
      Ok(#([var_declaration, ..statements], new_tokens))
    }
    _ ->
      Error(ParseError(
        message: "a variable declaration must be followed by an assignment or ';'.",
      ))
  }
}

fn var_declaration_with_assignment(
  name: Token,
  statements: List(Stmt),
  tokens: List(Token),
) -> LoxResult(StmtsAndTokens) {
  expression(tokens)
  |> then(fn(result) {
    let #(expr, [semicolon, ..new_tokens]) = result
    case semicolon.token_type {
      Semicolon -> {
        let var_declaration = VarDecl(name: name, initializer: expr)
        Ok(#([var_declaration, ..statements], new_tokens))
      }
      _ ->
        Error(ParseError(
          message: "a variable declaration with assignment must be followed by ';'.",
        ))
    }
  })
}

fn while_statement(
  statements: List(Stmt),
  tokens: List(Token),
) -> LoxResult(StmtsAndTokens) {
  let [first_token, ..new_tokens] = tokens
  case first_token.token_type {
    LeftParen -> while_condition_is_ok(statements, new_tokens)
    _ -> Error(ParseError(message: "expect '(' after 'while'."))
  }
}

fn while_condition_is_ok(
  statements: List(Stmt),
  tokens: List(Token),
) -> LoxResult(StmtsAndTokens) {
  tokens
  |> expression()
  |> then(fn(result) {
    let #(condition, [right_paren, ..new_tokens]) = result
    case right_paren.token_type {
      RightParen -> do_while_statement(condition, statements, new_tokens)
      _ -> Error(ParseError(message: "expect ')' after while condition."))
    }
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

fn block(
  existing_statements: List(Stmt),
  tokens: List(Token),
) -> LoxResult(StmtsAndTokens) {
  let assert Ok(first_token) = list.first(tokens)
  case first_token.token_type {
    Eof ->
      Error(ParseError(message: "unexpected end of file while parsing block."))
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

fn do_statement(statements: List(Stmt), tokens: List(Token), stmt_type) {
  tokens
  |> expression()
  |> then(fn(result) {
    let #(expr, [Token(token_type: Semicolon, ..), ..new_tokens]) = result
    let new_statement = stmt_type(expr)
    declaration(Ok(#([new_statement, ..statements], new_tokens)))
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
          // at `do_statement`
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
  other_tokens: List(Token),
) -> LoxResult(ExprAndTokens) {
  other_tokens
  |> expression()
  |> then(fn(result) {
    case result {
      #(inner_expr, [Token(value: LoxString(")"), ..), ..tokens2]) -> {
        let expr = Grouping(expression: inner_expr, line: first_token.line)
        Ok(#(expr, tokens2))
      }
      #(_inner_expr, [Token(value: not_right_paren, ..), ..]) -> {
        Error(ParseError(
          message: "unmatched '(', got " <> string.inspect(not_right_paren) <> " instead.",
        ))
      }
    }
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
