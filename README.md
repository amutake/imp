Imp
===

This is a simple imperative language for learning/teaching Haskell.

The [documents](./docs) are written in Japanese.

The implementation is about 350 lines.

Example
-------

```javascript
// comment
var x = 0;                // declare variable, number literal
var f = fun(x, y) {       // function literal
  return x + y;           // return statement
};
if (x != 0) {             // if statement (not expression)
  print("assert false\n"); // print, string literal
} else {
  while (x < 10) {        // while statement
    x = x + 1;            // assignment
  }
  print(f(x, 10));         // function application
}
```


Types
-----

- number
- boolean
- string


Operators
---------

- `(==), (!=) : 'a -> 'a -> boolean`
- `(&&), (||) : boolean -> boolean -> boolean`
- `(+), (-), (*), (/) : number -> number -> number`
- `(<), (<=), (>), (>=) : number -> number -> boolean`
- `(++) : string -> string -> string`


BNF
---

```
<toplevel> ::= <statement>*

<statement> ::= var <identifier> = <expression>;
              | <identifier> = <expression>;
              | if (<expression>) { <statement>* } else { <statement>* }
              | while (<expression>) { <statement>* }
              | return <expression>;
              | <expression>;

<expression> ::= <constant>
               | fun(<identifiers>) { <statement>* }
               | <expression> <operator> <expression>
               | <expression>(<expressions>)
               | <identifier>

<expressions> ::= <expression>, <expression>, ..., <expression>

<identifiers> ::= <identifier>, <identifier>, ..., <identifier>

<constant> ::= <integer or float>
             | true | false
             | " <any char without ">* "

<identifier> ::= [a-z][a-zA-Z_0-9'!?]*

<operator> ::= == | !=
             | && | ||
             | + | - | * | /
             | < | <= | > | >=
             | ++
```


Reserved idetifiers
-------------------

`var, if, else, while, return, fun, true, false`


Builtin functions
-----------------

- `number_to_string`
- `boolean_to_string`
- `number?`
- `string?`
- `boolean?`
- `not`
- `length`
- `print`
- `println`
