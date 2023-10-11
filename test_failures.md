## Regex to get a nice list of only the files

`.lox\n(.|\n)+?\n\n`

replace by `.lox\n`

## Chapter 9, Control
87 tests pass, 9 fail.

(1) test/for/statement_condition.lox
(2) test/for/statement_initializer.lox
(3) test/variable/unreached_undefined.lox
(4) test/while/class_in_body.lox
(5) test/while/var_in_body.lox
(6) test/while/fun_in_body.lox
(7) test/if/truth.lox
(8) test/if/if.lox
(9) test/if/dangling_else.lox

## Chapter 10, Functions
121 tests pass, 18 fail;

the nine from chapter 9,
and the following nine:

(10) test/function/nested_call_with_arguments.lox
(11) test/function/recursion.lox
(12) test/function/local_recursion.lox
(13) test/function/mutual_recursion.lox
(14) test/for/syntax.lox
(15) test/for/closure_in_body.lox
(16) test/closure/assign_to_closure.lox
(17) test/closure/nested_closure.lox
(18) test/unexpected_character.lox

## Future chapters

(Out of date)

Running the tests for the last chapter of clox (which includes some tests that don't make sense
to run with jlox), I get 103 passes and 143 failures, a total of 246 tests.
