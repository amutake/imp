Imp
===

This is a simple imperative language for learning Haskell.

The documents are written in Japanese.

Examples
--------

```javascript
// comment
var x = 0; // declare variable, number
var f = fun(x, y) { // function literal
  return x + y; // return statement
};
if (x == 0) { // if statement (not expression)
  print "assert false\n"; // print, string literal
} else {
  while (x < 10) { // while statement
    x = x + 1; // reassignment
  }
  print x;
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
              | print <expression>;
              | <expression>;

<expression> ::= <constant>
               | fun(<identifiers>) { <statement>* }
               | <expression> <operator> <expression>
               | <expression>(<expressions>)

<expressions> ::= <expression>, <expression>, ..., <expression>

<identifiers> ::= <identifier>, <identifier>, ..., <identifier>

<constant> ::= [0-9\.?0-9*]
             | true | false
             | "(.*\")"

<identifier> ::= [a-z][a-zA-Z_0-9'!?]

<operator> ::= == | !=
             | && | ||
             | + | - | * | /
             | < | <= | > | >=
             | ++
```


Reserved idetifiers
-------------------

`var, if, else, while, return, print, fun, true, false`


Builtin functions
-----------------

- `number_to_string`
- `boolean_to_string`
- `number?`
- `string?`
- `boolean?`
- `not`
- `length`
