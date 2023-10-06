## Chapter 8, Statements
68 tests pass, 1 fails:

(1) test/variable/shadow_local.lox

## Chapter 9, Control
78 tests pass, 18 fail.

The one from chapter 8, and the following:

(2) test/for/statement_condition.lox
(3) test/for/class_in_body.lox
(4) test/for/var_in_body.lox
(5) test/for/fun_in_body.lox
(6) test/for/scope.lox
(7) test/for/statement_initializer.lox
(8) test/while/syntax.lox
(9) test/while/var_in_body.lox
(10) test/while/fun_in_body.lox
(11) test/if/truth.lox
(12) test/if/fun_in_then.lox
(13) test/if/if.lox
(14) test/if/var_in_else.lox
(15) test/if/dangling_else.lox
(16) test/if/fun_in_else.lox
(17) test/if/else.lox
(18) test/if/var_in_then.lox

## Chapter 10, Functions
104 tests pass, 35 fail.

The one from chapter 8,
The seventeen from chapter 9,
And the following seventeen:

(19) test/function/nested_call_with_arguments.lox
(20) test/function/recursion.lox
(21) test/function/local_recursion.lox
(22) test/function/mutual_recursion.lox
(23) test/for/syntax.lox
(24) test/for/return_inside.lox
(25) test/for/return_closure.lox
(26) test/for/closure_in_body.lox
(27) test/while/closure_in_body.lox
(28) test/closure/assign_to_closure.lox
(29) test/closure/shadow_closure_with_local.lox
(30) test/closure/nested_closure.lox
(31) test/closure/reuse_closure_slot.lox
(32) test/unexpected_character.lox
(33) test/return/after_while.lox
(34) test/return/after_if.lox
(35) test/return/after_else.lox

## Future chapters

Running the tests for the last chapter of clox (which includes some tests that don't make sense
to run with jlox), I get 103 passes and 143 failures, a total of 246 tests.
