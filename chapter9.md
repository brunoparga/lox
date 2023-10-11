# Chapter 9 test report

9 errors live in this chapter
9 live in chapter 10

## Error 1/18

### File

FAIL test/for/statement_condition.lox

### Test source

```
// [line 3] Error at '{': Expect expression.
// [line 3] Error at ')': Expect ';' after expression.
for (var a = 1; {}; a = a + 1) {}
```

### Test output

Missing expected error: [3] Error at ')': Expect ';' after expression.

### Diagnosis

As things stand, the program only has the ability to throw one error at a time, where the test
expects two different errors. I don't understand the second error it expects to be thrown; there
didn't need to be a semicolon right before the closing parenthesis.

On a very positive note, I've discovered and patched a trivial off-by-one error, which in turn
passed a new previously failing test.

Update: what happens is that the Java parser enters panic mode when the first parse error is
encountered. This causes it to return `NULL` where it gets an empty block and it expected an
expression. This means it forgets it is parsing a for loop (due to the `synchronize` method) and
this causes it to throw the error after the incrementer.

I currently don't do parser panic and synchronization.

## Error 3/34

### File

FAIL test/for/class_in_body.lox

### Test source

```
// [line 2] Error at 'class': Expect expression.
for (;;) class Foo {}
```

### Test output

Unexpected error:
[line 2] Error at ')': Expect expression.
Missing expected error: [2] Error at 'class': Expect expression.

### Diagnosis

There was an error in consuming the semicolon of an empty initializer.

## Error 4 (FIXED)

### File

test/for/var_in_body.lox

### Test source

```
// [line 2] Error at 'var': Expect expression.
for (;;) var foo;
```

### Test output

Unexpected error:
[line 2] Error at ')': Expect expression.
Missing expected error: [2] Error at 'var': Expect expression.

### Diagnosis

Not needed, as the test is FIXED.

## Error 5/34

### File

test/for/fun_in_body.lox

### Test source

```
// [line 2] Error at 'fun': Expect expression.
for (;;) fun foo() {}
```

### Test output

Missing expected error: [2] Error at 'fun': Expect expression.
Expected return code 65 and got 127. Stderr:
Got output 'exception error: no match of right hand side value []' when none was expected.
Got output '  in function  lox_gleam@parser:statement/2 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 110)' when none was expected.
Got output '  in call from lox_gleam@parser:parse/1 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 191)' when none was expected.
Got output '  in call from lox_gleam:run/2 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam.erl, line 12)' when none was expected.

### Diagnosis

The diagnosis was useful at least for one thing: I know for sure now I can have an empty function
body. OTOH, the body of a for statement is a list, and it probably shouldn't. Correction: it is
that annoying list of statements that I pass all around.

As for the issue of this error, it is because the parser's `declaration` function doesn't return
when the list of tokens is empty, but it recurses instead.

## Error 6/34

### File

test/for/scope.lox

### Test source

```
{
  var i = "before";

  // New variable is in inner scope.
  for (var i = 0; i < 1; i = i + 1) {
    print i; // expect: 0

    // Loop body is in second inner scope.
    var i = -1;
    print i; // expect: -1
  }
}

{
  // New variable shadows outer variable.
  for (var i = 0; i > 0; i = i + 1) {}

  // Goes out of scope after loop.
  var i = "after";
  print i; // expect: after

  // Can reuse an existing variable.
  for (i = 0; i < 1; i = i + 1) {
    print i; // expect: 0
  }
}
```

### Attempt 1

#### 6.1 Test output

Expected return code 0 and got 127. Stderr:
Expected output '0' on line 6  and got 'exception error: no match of right hand side value '.
Expected output '-1' on line 10  and got '                 {{assign,<<"23">>,'.
Expected output 'after' on line 20  and got '                          {lox_string,<<"i">>},'.
Expected output '0' on line 24  and got '                          {binary,<<"23">>,plus,'.
Got output '                                  {variable,<<"23">>,{lox_string,<<"i">>}},' when none was expected.
Got output '                                  {literal,<<"23">>,{lox_number,1.0}}}},' when none was expected.
Got output '                  [{token,<<"23">>,right_paren,{lox_string,<<")">>}},' when none was expected.
Got output '                   {token,<<"23">>,left_brace,{lox_string,<<"{">>}},' when none was expected.
Got output '                   {token,<<"24">>,print,{lox_string,<<"print">>}},' when none was expected.
Got output '                   {token,<<"24">>,identifier,{lox_string,<<"i">>}},' when none was expected.
Got output '                   {token,<<"24">>,semicolon,{lox_string,<<";">>}},' when none was expected.
Got output '                   {token,<<"25">>,right_brace,{lox_string,<<"}">>}},' when none was expected.
Got output '                   {token,<<"26">>,right_brace,{lox_string,<<"}">>}},' when none was expected.
Got output '                   {token,<<"27">>,eof,lox_nil}]}' when none was expected.
Got output '  in function  lox_gleam@parser:'-basic_statement/3-fun-0-'/3 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 728)' when none was expected.
Got output '  in call from lox_gleam@parser:for_stmt_initializer/2 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 986)' when none was expected.
Got output '  in call from lox_gleam@parser:for_statement/2 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 1000)' when none was expected.
Got output '  in call from lox_gleam@parser:block/2 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 249)' when none was expected.
Got output '  in call from lox_gleam@parser:parse/1 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 191)' when none was expected.
Got output '  in call from lox_gleam@parser:'-parse/1-fun-2-'/1 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 211)' when none was expected.
Got output '  in call from lox_gleam:run/2 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam.erl, line 12)' when none was expected.

#### 6.1 Diagnosis

The parser at for_stmt_increment currently consumes the right paren implicitly. It'd be better to
make that implicit, in line with the idea of explicit creation of statements and expressions.

The problem that makes the BEAM crash is that reassignment of a variable consumes the semicolon,
and the for initializer also tries to do that. I need to standardize who consumes what. I'll do
a gambi for the time being to see if it clears up this test. Scratch that; there *is* no semicolon
in this situation because it's a reassignment as the increment of a for loop.

The consumption of that semicolon needs to be the responsibility of whomever generates the statement.

### Attempt 2

#### Changes

When a `basic_statement` is constructed, I check if there is a semicolon before consuming it. This
is a gambiarra and should be avoided.

#### 6.2 Test output

Unexpected error:
[line 23] Error at ')': Expect expression.
Expected return code 0 and got 65. Stderr:
[line 23] Error at ')': Expect expression.
Missing expected output '0' on line 6.
Missing expected output '-1' on line 10.
Missing expected output 'after' on line 20.
Missing expected output '0' on line 24.

#### 6.2 Diagnosis

`basic_statement` also needs to chew up the right paren, in case it is dealing with a for statement
increment. I've extended the gambi to do just that.

### Attempt 3

#### 6.3 Test output

Unexpected error:
[line 26] Error at '}': Expect expression.
Expected return code 0 and got 65. Stderr:
[line 26] Error at '}': Expect expression.
Missing expected output '0' on line 6.
Missing expected output '-1' on line 10.
Missing expected output 'after' on line 20.
Missing expected output '0' on line 24.

#### 6.3 Diagnosis

Replicating the gambiarra with the RightBrace token did not change the test output.

The error happens at (before) `for_stmt_condition`, seemingly. The tokens it gets are just the
semicolon to end the whole block and the EOF.

Actually, the problem is that `for_stmt_initializer` calls `basic_statement`, which calls
`declaration`. This is the same big problem of functions doing too much (calling the parent recursive
function instead of just returning the statement/declaration/expression they're supposed to).

## Error 7/34

### File

test/for/statement_initializer.lox

### Test source

```
// [line 3] Error at '{': Expect expression.
// [line 3] Error at ')': Expect ';' after expression.
for ({}; a < 2; a = a + 1) {}
```

### Test output

Missing expected error: [3] Error at ')': Expect ';' after expression.

### Diagnosis

Like with error number 2, my code cannot yet handle more than one error at a time. Still, I am not
sure why the second error is supposed to be thrown.

## Error 8/34

test/while/syntax.lox

### File

### Test source

```
// Single-expression body.
var c = 0;
while (c < 3) print c = c + 1;
// expect: 1
// expect: 2
// expect: 3

// Block body.
var a = 0;
while (a < 3) {
  print a;
  a = a + 1;
}
// expect: 0
// expect: 1
// expect: 2

// Statement bodies.
while (false) if (true) 1; else 2;
while (false) while (true) 1;
while (false) for (;;) 1;
```

### Test output

Expected return code 0 and got 127. Stderr:
Expected output '1' on line 4  and got 'exception error: no match of right hand side value []'.
Expected output '2' on line 5  and got '  in function  lox_gleam@parser:statement/2 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 110)'.
Expected output '3' on line 6  and got '  in call from lox_gleam@parser:do_while_statement/3 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 475)'.
Expected output '0' on line 14  and got '  in call from lox_gleam@parser:if_then_else/4 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 405)'.
Expected output '1' on line 15  and got '  in call from lox_gleam@parser:do_while_statement/3 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 475)'.
Expected output '2' on line 16  and got '  in call from lox_gleam@parser:parse/1 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 191)'.
Got output '  in call from lox_gleam:run/2 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam.erl, line 12)' when none was expected.

### Diagnosis

Previously this threw only "expression expected". I miss the old error.

I believe the error is also due to the recursion runoff.

## Error 9/34

test/while/var_in_body.lox

### File

### Test source

```
// [line 2] Error at 'var': Expect expression.
while (true) var foo;
```

### Test output

Missing expected error: [2] Error at 'var': Expect expression.
Expected return code 65 and got 127. Stderr:
Got output 'exception error: no match of right hand side value []' when none was expected.
Got output '  in function  lox_gleam@parser:statement/2 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 110)' when none was expected.
Got output '  in call from lox_gleam@parser:parse/1 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 191)' when none was expected.
Got output '  in call from lox_gleam:run/2 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam.erl, line 12)' when none was expected.

### Diagnosis

This is another test which would at least change if `declaration` handled an empty token list. Let's
see what happens if I do that...

... it's obvious in hindsight: `declaration` eventually calls itself, so the way I'd done this before
gets into an infinite loop. Let's try something different...

... I don't think it's possible to return a value that cannot get into an infinite loop. This issue
will only go away when functions don't mutually recurse with `declaration`.

## Error 10/34

### File

test/while/fun_in_body.lox

### Test source

```
// [line 2] Error at 'fun': Expect expression.
while (true) fun foo() {}
```

### Test output

Missing expected error: [2] Error at 'fun': Expect expression.
Expected return code 65 and got 127. Stderr:
Got output 'exception error: no match of right hand side value []' when none was expected.
Got output '  in function  lox_gleam@parser:statement/2 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 110)' when none was expected.
Got output '  in call from lox_gleam@parser:parse/1 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 191)' when none was expected.
Got output '  in call from lox_gleam:run/2 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam.erl, line 12)' when none was expected.

### Diagnosis

I suspect recursion runoff.

## Error 11/34

### File

test/if/truth.lox

### Test source

```
// False and nil are false.
if (false) print "bad"; else print "false"; // expect: false
if (nil) print "bad"; else print "nil"; // expect: nil

// Everything else is true.
if (true) print true; // expect: true
if (0) print 0; // expect: 0
if ("") print "empty"; // expect: empty
```

### Test output

Expected return code 0 and got 127. Stderr:
Expected output 'false' on line 2  and got 'exception error: no match of right hand side value []'.
Expected output 'nil' on line 3  and got '  in function  lox_gleam@parser:statement/2 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 110)'.
Expected output 'true' on line 6  and got '  in call from lox_gleam@parser:do_if_statement/3 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 442)'.
Expected output '0' on line 7  and got '  in call from lox_gleam@parser:if_then_else/4 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 405)'.
Expected output 'empty' on line 8  and got '  in call from lox_gleam@parser:parse/1 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 191)'.
Got output '  in call from lox_gleam:run/2 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam.erl, line 12)' when none was expected.

### Diagnosis

Yup, also recursion runoff.

## Error 12/34

### File

test/if/fun_in_then.lox

### Test source

```
// [line 2] Error at 'fun': Expect expression.
if (true) fun foo() {}
```

### Test output

Missing expected error: [2] Error at 'fun': Expect expression.
Expected return code 65 and got 127. Stderr:
Got output 'exception error: no match of right hand side value []' when none was expected.
Got output '  in function  lox_gleam@parser:statement/2 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 110)' when none was expected.
Got output '  in call from lox_gleam@parser:parse/1 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 191)' when none was expected.
Got output '  in call from lox_gleam:run/2 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam.erl, line 12)' when none was expected.

### Diagnosis

Recursion runoff. At this point I'm just eager to go and fix that problem. At least it seems that'll
give me a lot of bang for the debugging buck.

## Error 13/34

### File

test/if/if.lox

### Test source

```
// Evaluate the 'then' expression if the condition is true.
if (true) print "good"; // expect: good
if (false) print "bad";

// Allow block body.
if (true) { print "block"; } // expect: block

// Assignment in if condition.
var a = false;
if (a = true) print a; // expect: true
```

### Test output

Expected return code 0 and got 127. Stderr:
Expected output 'good' on line 2  and got 'exception error: no match of right hand side value []'.
Expected output 'block' on line 6  and got '  in function  lox_gleam@parser:statement/2 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 110)'.
Expected output 'true' on line 10  and got '  in call from lox_gleam@parser:do_if_statement/3 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 442)'.
Got output '  in call from lox_gleam@parser:parse/1 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 191)' when none was expected.
Got output '  in call from lox_gleam:run/2 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam.erl, line 12)' when none was expected.

### Diagnosis

Very vague hypothesis: I require the body to be a block, but I should allow any statement instead.
(The thing I now want to do anyway will hopefully solve this.)

## Error 14/34

test/if/var_in_else.lox

### File

### Test source

```
// [line 2] Error at 'var': Expect expression.
if (true) "ok"; else var foo;
```

### Test output

Missing expected error: [2] Error at 'var': Expect expression.
Expected return code 65 and got 127. Stderr:
Got output 'exception error: no match of right hand side value []' when none was expected.
Got output '  in function  lox_gleam@parser:statement/2 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 110)' when none was expected.
Got output '  in call from lox_gleam@parser:parse/1 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 191)' when none was expected.
Got output '  in call from lox_gleam:run/2 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam.erl, line 12)' when none was expected.

### Diagnosis

Same.

## Error 15/34

### File

test/if/dangling_else.lox

### Test source

```
// A dangling else binds to the right-most if.
if (true) if (false) print "bad"; else print "good"; // expect: good
if (false) if (true) print "bad"; else print "bad";
```

### Test output

Expected return code 0 and got 127. Stderr:
Expected output 'good' on line 2  and got 'exception error: no match of right hand side value []'.
Got output '  in function  lox_gleam@parser:statement/2 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 110)' when none was expected.
Got output '  in call from lox_gleam@parser:do_if_statement/3 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 442)' when none was expected.
Got output '  in call from lox_gleam@parser:if_then_else/4 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 405)' when none was expected.
Got output '  in call from lox_gleam@parser:do_if_statement/3 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 442)' when none was expected.
Got output '  in call from lox_gleam@parser:parse/1 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 191)' when none was expected.
Got output '  in call from lox_gleam:run/2 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam.erl, line 12)' when none was expected.

### Diagnosis

Same.

## Error 16/34

### File

test/if/fun_in_else.lox

### Test source

```
// [line 2] Error at 'fun': Expect expression.
if (true) "ok"; else fun foo() {}
```

### Test output

Missing expected error: [2] Error at 'fun': Expect expression.
Expected return code 65 and got 127. Stderr:
Got output 'exception error: no match of right hand side value []' when none was expected.
Got output '  in function  lox_gleam@parser:statement/2 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 110)' when none was expected.
Got output '  in call from lox_gleam@parser:parse/1 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 191)' when none was expected.
Got output '  in call from lox_gleam:run/2 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam.erl, line 12)' when none was expected.

### Diagnosis

Same.

## Error 17/34

### File

test/if/else.lox

### Test source

```
// Evaluate the 'else' expression if the condition is false.
if (true) print "good"; else print "bad"; // expect: good
if (false) print "bad"; else print "good"; // expect: good

// Allow block body.
if (false) nil; else { print "block"; } // expect: block
```

### Test output

Expected return code 0 and got 127. Stderr:
Expected output 'good' on line 2  and got 'exception error: no match of right hand side value []'.
Expected output 'good' on line 3  and got '  in function  lox_gleam@parser:statement/2 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 110)'.
Expected output 'block' on line 6  and got '  in call from lox_gleam@parser:if_then_else/4 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 405)'.
Got output '  in call from lox_gleam@parser:parse/1 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 191)' when none was expected.
Got output '  in call from lox_gleam:run/2 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam.erl, line 12)' when none was expected.

### Diagnosis

Samesies.

## Error 18/34

### File

test/if/var_in_then.lox

### Test source

```
// [line 2] Error at 'var': Expect expression.
if (true) var foo;
```

### Test output

Missing expected error: [2] Error at 'var': Expect expression.
Expected return code 65 and got 127. Stderr:
Got output 'exception error: no match of right hand side value []' when none was expected.
Got output '  in function  lox_gleam@parser:statement/2 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 110)' when none was expected.
Got output '  in call from lox_gleam@parser:parse/1 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam@parser.erl, line 191)' when none was expected.
Got output '  in call from lox_gleam:run/2 (/home/bruno/code/learn/lox/gleam/build/prod/erlang/lox_gleam/_gleam_artefacts/lox_gleam.erl, line 12)' when none was expected.

### Diagnosis

Yep, they're all the same. I'll try to fix this before looking at the next chapter's tests.
