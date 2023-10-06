# Chapter 8 test report

1 error lives in this chapter
16 live in chapter 9
17 live in chapter 10

## Error 1/34

### File

test/variable/shadow_local.lox

### Test source

```
{
  var a = "local";
  {
    var a = "shadow";
    print a; // expect: shadow
  }
  print a; // expect: local
}
```

### Test output

Unexpected output on stderr:
Undefined variable 'a'.
Unexpected output on stderr:
[line 7]
Expected return code 0 and got 70. Stderr:
Undefined variable 'a'.
[line 7]
Missing expected output 'local' on line 7.

### Diagnosis

The code seems misparsed:

```
{block,<<"2">>,
      [{var_decl,<<"2">>,
                  {lox_string,<<"a">>},
                  {literal,<<"2">>,{lox_string,<<"local">>}}},
        {block,<<"4">>,
              [{var_decl,<<"4">>,
                          {lox_string,<<"a">>},
                          {literal,<<"4">>,{lox_string,<<"shadow">>}}},
                {print_stmt,<<"5">>,{variable,<<"5">>,{lox_string,<<"a">>}}}]}]},
{print_stmt,<<"7">>,{variable,<<"7">>,{lox_string,<<"a">>}}}
```
It is showing the second `print` statement as outside of any blocks, when it should be inside the
outer-level block.
