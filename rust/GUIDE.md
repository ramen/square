# The Square Programming Language

Square is an experimental programming language — a hybrid of ideas from
Scheme, ML, Python, and Arc. It features first-class functions, closures,
pattern matching, records, generics, and an interactive REPL.

## Getting Started

**Build:**

```
cd rust
cargo build --release
```

**Run a script:**

```
./target/release/square myscript.sq
```

**Start the REPL:**

```
./target/release/square
Welcome to [s]quare version 0.2.3!
:: println [+ 2 3];
5
```

The REPL supports line editing, history (up/down arrows), and standard
readline shortcuts (Ctrl-A, Ctrl-E, Ctrl-W, etc.). Press Ctrl-D to exit.

Scripts can also be piped via stdin:

```
echo 'println "hello";' | ./target/release/square -
```

---

## Syntax Overview

Square uses a **prefix** style for function application: the function comes
first, followed by its arguments. Parentheses are used only for creating
lists — *not* for grouping function calls.

```
+ 2 3           (* returns 5 *)
* 4 [+ 1 2]     (* returns 12 — square brackets for grouping *)
```

### Comments

```
(* This is a block comment. (* They nest. *) *)
# This is a line comment.
```

### Semicolons

Top-level statements are separated by semicolons. Semicolons inside
do-blocks separate sequential expressions.

```
println "hello";
println "world";
```

---

## Values and Types

Square has nine types:

| Type       | Examples                              | Type symbol  |
|------------|---------------------------------------|--------------|
| None       | `[]`                                  | `.none`      |
| Symbol     | `.true`, `.false`, `.foo`             | `.symbol`    |
| Int        | `42`, `-7`, `0`                       | `.int`       |
| Float      | `3.14`, `1e10`, `-0.5`                | `.float`     |
| Char       | `'a'`, `'\n'`, `'\t'`                 | `.char`      |
| String     | `"hello"`, `"line\n"`                 | `.string`    |
| List       | `()`, `(1, 2, 3)`, `("a", "b")`      | `.list`      |
| Record     | `{}`, `{name: "Alice", age: 30}`      | `.record`    |
| Function   | `fun x -> * x 2`                      | `.function`  |

### None

`[]` represents the absence of a value. It is returned by `def`, `:=`,
and `print`, among others. The empty list is `()`, not `[]`.

### Symbols

Symbols are named constants, written with a leading dot. They are compared
by identity and are commonly used as tags, enums, and boolean values.

```
.true
.false
.foo
```

The booleans are `.true` and `.false`. Most values have a boolean
interpretation — see **Truthiness** below.

### Numbers

Integers and floats. Arithmetic operations promote integers to floats
automatically when mixed.

```
42
3.14
-7
1e10
```

### Characters

Single characters, written with single quotes:

```
'a'
'\n'
'\t'
```

### Strings

Double-quoted, with standard escape sequences (`\n`, `\t`, `\\`, `\"`):

```
"hello, world"
"line 1\nline 2"
```

Strings are indexable (zero-based): `"hello" 0` returns `'h'`.

### Lists (Tuples)

Comma-separated values in parentheses. Lists are ordered, indexable
sequences. The empty list is `()`.

```
(1, 2, 3)
("a", "b", "c")
(1, "mixed", .types)
()
```

Lists are indexable: `(10, 20, 30) 1` returns `20`. Negative indices count
from the end: `(10, 20, 30) -1` returns `30`.

### Records

Key-value pairs in curly braces. Keys are names; values can be any type.

```
{name: "Alice", age: 30}
{x: 1, y: 2}
{}
```

Access fields using symbol application:

```
def person {name: "Alice", age: 30};
person .name      (* "Alice" *)
person .age       (* 30 *)
```

Records can be tagged to create custom types — see **Generics** below.

---

## Expressions

### Do-blocks `[...]`

Square brackets delimit a *do-block*: a sequence of expressions separated
by semicolons. The value of the block is the value of its last expression.

```
[println "step 1"; println "step 2"; + 1 2]
(* prints "step 1" and "step 2", returns 3 *)
```

A single expression in brackets is just a grouping mechanism:

```
* 2 [+ 3 4]    (* 14 *)
```

The `do` keyword can also be used: `do [...]`.

### Function application

Application is written in prefix notation and is **left-associative**:

```
f x y    (* means: (f x) y — f applied to x, result applied to y *)
```

This means multi-argument functions are **curried** — each function takes
one argument and returns a new function:

```
+ 2 3        (* + takes 2, returns a function, that function takes 3 *)
```

To pass a computed expression as an argument, wrap it in brackets:

```
println [+ 2 3];     (* prints 5 — the result of [+ 2 3] is passed to println *)
println + 2 3;       (* WRONG — this tries to apply println to +, then to 2... *)
```

### Definitions `def`

`def` binds a name in the current scope:

```
def x 42;
def greet fun name -> join "" ("Hello, ", name, "!");
```

Unlike `let`, `def` mutates the current environment, so later definitions
and closures in the same scope can see each other.

### Assignment `:=`

Mutates an existing binding:

```
def counter 0;
:= counter [+ counter 1];
println counter;    (* 1 *)
```

### Undefine `undef`

Removes a binding from the current scope:

```
def x 42;
undef x;
println x;    (* Error: NameError *)
```

### Let bindings `let`

`let` introduces local bindings that are visible only within its body expression:

```
let {x: 10, y: 20} + x y
(* returns 30 — x and y are not visible outside *)
```

### Letrec `letrec`

Like `let`, but bindings can refer to each other (for mutual recursion):

```
letrec {
  even: fun n -> if = n 0 then .true else odd [- n 1],
  odd:  fun n -> if = n 0 then .false else even [- n 1],
}
even 10
```

### Conditionals `if`/`then`/`elif`/`else`

```
if > x 0
then "positive"
elif = x 0
then "zero"
else "negative"
```

Without `else`, a false condition returns `[]` (None).

### Short-circuit logic `and`/`or`

```
and (> x 0, < x 100)     (* true if both conditions hold *)
or (= x 0, = x 1)        (* true if either condition holds *)
```

These are not functions — they short-circuit evaluation.

### Truthiness

Most values can be used as booleans:

| Value                  | Boolean   |
|------------------------|-----------|
| `[]` (None)            | false     |
| `.false`               | false     |
| `0` (int)              | false     |
| `0.0` (float)          | false     |
| `""` (empty string)    | false     |
| `()` (empty list)      | false     |
| `{}` (empty record)    | false     |
| Everything else        | **true**  |

---

## Functions

### Defining functions

Functions are defined with `fun`, a pattern for the argument, `->`, and a
body expression:

```
def double fun x -> * 2 x;
double 21    (* 42 *)
```

Since application is left-associative and functions are curried, a
multi-argument function is a function that returns a function:

```
def add fun x -> fun y -> + x y;
add 10 20    (* 30 *)
```

### Patterns

Functions can destructure their argument with patterns:

**Scalar** — binds the whole argument:

```
fun x -> * x x
```

**Empty** — accepts only `[]` (None), takes no argument:

```
fun [] -> println "called with no arg"
```

**List** — destructures a list:

```
def swap fun (a, b) -> (b, a);
swap (1, 2)    (* (2, 1) *)

def sum3 fun (a, b, c) -> + a [+ b c];
sum3 (1, 2, 3)    (* 6 *)
```

**Record** — destructures a record by field names:

```
def greet fun {name} -> join "" ("Hello, ", name, "!");
greet {name: "Alice", age: 30}    (* "Hello, Alice!" *)
```

### Closures

Functions capture their enclosing environment:

```
def make_adder fun n -> fun x -> + n x;
def add5 make_adder 5;
add5 10    (* 15 *)
```

### The `!` mutator syntax

`!` is shorthand for applying a function and assigning the result back.
`!f x` is equivalent to `:= x [f x ...]`:

```
def items ();
!cons items 1;
!cons items 2;
println items;    (* prints: 21 — items is now (2, 1) *)
```

---

## Error Handling

### Throwing errors

`throw` raises an error. By convention, errors are records:

```
throw {MyError: "something went wrong"}
```

### Catching errors

`try`/`catch` handles errors. Catch clauses can match by name or pattern:

**Match by name** — binds a named variable:

```
try throw "oops"
catch e -> println [join "" ("caught: ", string e)]
```

**Match by record pattern** — matches fields of a record error:

```
try throw {MyError: "oops"}
catch {MyError} -> println [join "" ("caught: ", MyError)]
```

Multiple `catch` clauses:

```
try some_function []
catch {IOError} -> println IOError
catch {TypeError} -> println TypeError
catch e -> println [join "" ("unknown: ", string e)]
```

If no catch matches, the error propagates.

---

## The Prelude

Square comes with a standard library of built-in functions, some
implemented natively and some written in Square itself.

### I/O

| Function    | Description                                            |
|-------------|--------------------------------------------------------|
| `print x`   | Print a value (lists print elements without separators)|
| `println x`  | Print a value followed by a newline                   |
| `read_line []` | Read a line from stdin                              |

### Type conversion

| Function    | Description                                |
|-------------|--------------------------------------------|
| `string x`  | Convert any value to its string form       |
| `int x`     | Convert to integer                         |
| `float x`   | Convert to float                           |
| `char x`    | Convert to character                       |
| `bool x`    | Convert to boolean (`.true` or `.false`)   |
| `symbol x`  | Convert string to symbol                   |
| `symbol_name x` | Get the name of a symbol as a string  |

### Type checking

| Function    | Description                                            |
|-------------|--------------------------------------------------------|
| `typeof x`  | Returns the type symbol (`.int`, `.string`, etc.)      |
| `isa type x`| Check if `x` is of a given type: `isa .int 42`        |
| `is a b`    | Identity comparison (same object, not structural)      |

### Comparison

| Function       | Description             |
|----------------|-------------------------|
| `= a b`        | Structural equality     |
| `<> a b`       | Not equal               |
| `< a b`        | Less than               |
| `> a b`        | Greater than            |
| `<= a b`       | Less than or equal      |
| `>= a b`       | Greater than or equal   |
| `compare (a,b)`| Returns `.<`, `.=`, `.>`|

### Arithmetic

| Function  | Description                                     |
|-----------|-------------------------------------------------|
| `+ a b`   | Addition (works on int and float)               |
| `- a b`   | Subtraction                                     |
| `* a b`   | Multiplication                                  |
| `/ a b`   | Division                                        |
| `% a b`   | Modulo                                          |
| `** a b`  | Exponentiation                                  |
| `abs x`   | Absolute value                                  |
| `neg x`   | Negation                                        |
| `sum xs`  | Sum of a list                                   |
| `product xs` | Product of a list                            |

### Logic

| Function  | Description                              |
|-----------|------------------------------------------|
| `not x`   | Logical negation                         |
| `and (a, b, ...)` | Short-circuit AND              |
| `or (a, b, ...)`  | Short-circuit OR               |

### List operations

| Function         | Description                                        |
|------------------|----------------------------------------------------|
| `cons x list`    | Prepend `x` to a list                              |
| `head list`      | First element                                      |
| `tail list`      | All elements except the first                      |
| `size xs`        | Length (works on strings, lists, records)           |
| `empty xs`       | True if size is 0                                  |
| `reverse xs`     | Reverse a list or string                           |
| `append xs`      | Flatten a list of lists                            |
| `add xs x`       | Append `x` to end of list `xs`                     |
| `sort xs`        | Sort a list                                        |
| `sort_with f xs` | Sort with custom comparator                        |
| `in xs x`        | Check membership                                   |
| `index xs x`     | Find index of `x` in `xs`                          |
| `slice (start, stop) xs` | Slice a list or string (negative ok)      |

### Higher-order functions

| Function          | Description                                       |
|-------------------|---------------------------------------------------|
| `map xs f`        | Apply `f` to each element, return list             |
| `mapi xs f`       | Like `map` but `f` receives `(index, value)`       |
| `filter xs f`     | Keep elements where `f` returns true               |
| `filteri xs f`    | Like `filter` but `f` receives `(index, value)`    |
| `fold xs f init`  | Fold/reduce; `f` receives `(index, value, acc)`    |
| `each xs f`       | Apply `f` to each element (for side effects)       |
| `eachi xs f`      | Like `each` but `f` receives `(index, value)`      |
| `compose (f, g)`  | Function composition: `compose (f, g)` = `f(g(x))` |
| `flip f`          | Swap arguments: `flip f x y` = `f y x`            |
| `id x`            | Identity function                                  |

### String operations

| Function               | Description                              |
|------------------------|------------------------------------------|
| `join sep list`        | Join list elements with separator        |
| `split delim str`      | Split string by delimiter                |
| `lowercase s`          | Lowercase a string or char               |
| `uppercase s`          | Uppercase a string or char               |
| `starts_with prefix s` | Test if string starts with prefix        |
| `ends_with suffix s`   | Test if string ends with suffix          |
| `replace from to s`    | Replace all occurrences                  |
| `size s`               | String length                            |

### Record operations

| Function           | Description                                   |
|--------------------|-----------------------------------------------|
| `rec .field`       | Access a field (apply record to symbol)        |
| `update rec1 rec2` | Merge `rec2` into `rec1`                       |
| `remove rec .field`| Remove a field                                 |
| `add rec .field val`| Add a field                                   |
| `in rec .field`    | Check if field exists                          |
| `fields rec`       | List of field name symbols                     |
| `values rec`       | List of values                                 |
| `pairs rec`        | List of `(key, value)` pairs                   |
| `tag .name rec`    | Tag a record with a custom type symbol         |
| `typeof tagged`    | Returns the tag symbol                         |

### Iteration

| Function         | Description                                      |
|------------------|--------------------------------------------------|
| `range args`     | Generate a list of numbers.                      |
|                  | `range {stop: 5}` → `(0, 1, 2, 3, 4)`           |
|                  | `range {start: 1, stop: 5}` → `(1, 2, 3, 4)`    |
|                  | `range {start: 0, stop: 10, step: 2}` → `(0, 2, 4, 6, 8)` |
| `forever f`      | Call `f []` in an infinite loop (exit via throw)  |

### Mutable references

| Function      | Description                                       |
|---------------|---------------------------------------------------|
| `ref value`   | Create a mutable reference                        |
| `@ r`         | Read the value of a ref                           |
| `@= r value`  | Set the value of a ref                            |

```
def counter ref 0;
@= counter [+ [@ counter] 1];
println [@ counter];    (* 1 *)
```

### Hashing

| Function    | Description                     |
|-------------|---------------------------------|
| `hash x`    | Returns a hash code (integer)   |

---

## Modules

Modules are records tagged with `.module`. They are accessed using field
syntax.

### Math

| Function / Constant | Description                         |
|---------------------|-------------------------------------|
| `Math.pi`           | π ≈ 3.14159                         |
| `Math.e`            | Euler's number ≈ 2.71828            |
| `Math.Phi`          | Golden ratio ≈ 1.61803             |
| `Math.phi`          | 1/Phi ≈ 0.61803                    |
| `Math.sqrt x`       | Square root                         |
| `Math.sin x`        | Sine                                |
| `Math.cos x`        | Cosine                              |
| `Math.tan x`        | Tangent                             |
| `Math.asin x`       | Arcsine                             |
| `Math.acos x`       | Arccosine                           |
| `Math.atan x`       | Arctangent                          |
| `Math.atan2 y x`    | Two-argument arctangent             |
| `Math.exp x`        | Exponential                         |
| `Math.log x`        | Natural logarithm                   |
| `Math.log10 x`      | Base-10 logarithm                   |
| `Math.ceil x`       | Ceiling                             |
| `Math.floor x`      | Floor                               |
| `Math.sinh x`       | Hyperbolic sine                     |
| `Math.cosh x`       | Hyperbolic cosine                   |
| `Math.tanh x`       | Hyperbolic tangent                  |

### File

| Function             | Description                                |
|----------------------|--------------------------------------------|
| `File.open_in path`  | Open for reading; returns a file record    |
| `File.open_out path` | Open for writing; returns a file record    |

File records have `.read []` (returns contents as string), `.write s`, and
`.close []` methods:

```
def f File.open_in "data.txt";
def contents f.read [];
f.close [];
```

### OS

| Function            | Description                       |
|---------------------|-----------------------------------|
| `OS.args`           | Command-line arguments (list)     |
| `OS.getcwd []`      | Current working directory         |
| `OS.chdir path`     | Change directory                  |
| `OS.listdir path`   | List directory contents           |
| `OS.system cmd`     | Run a shell command               |

### RE (Regular Expressions)

| Function           | Description                                    |
|--------------------|------------------------------------------------|
| `RE.quote s`       | Escape a string for use in a regex             |
| `RE.compile pat`   | Compile a regex; returns an RE record          |

RE records have `.search str` and `.replace repl str` methods:

```
def re RE.compile "([0-9]+)";
def m re.search "abc123def";
println m.start;      (* 3 *)
println m.groups;     (* ("123") — list of match groups *)

println [re.replace "NUM" "abc123def"];    (* "abcNUMdef" *)
```

### Time

| Function         | Description                             |
|------------------|-----------------------------------------|
| `Time.time []`   | Current Unix timestamp as a float       |

---

## Generics

Generics provide ad-hoc polymorphism — the ability to define functions
that behave differently based on the *type tag* of their argument.

### Using generics

When you tag a record, its type becomes the tag symbol instead of
`.record`. Generic functions dispatch on this type:

```
def Point tag .point {x: 3, y: 4};
typeof Point          (* .point *)
string Point          (* by default, "<point>" *)
```

### Defining generic implementations

Use `def_generic` to register an implementation of a generic function
for a specific type tag:

```
def_generic .to_string .point fun p ->
  join "" ("Point(", string p.x, ", ", string p.y, ")");

println [string Point];    (* "Point(3, 4)" *)
```

The first argument is the generic function name (as a symbol), the
second is the type tag, and the third is the implementation.

### Built-in generics

| Generic       | Description                                       |
|---------------|---------------------------------------------------|
| `.to_string`  | Used by `string` to convert to a string           |
| `.reverse`    | Used by `reverse`                                 |
| `.call`       | Called when a record is applied as a function      |
| `.fold`       | Called when `fold` is used on a tagged record      |
| `.compare`    | Called when comparing tagged records               |

### The `generic` helper

The prelude provides a `generic` helper that creates a function which
dispatches via the generics table:

```
def reverse generic .reverse;
(* Now `reverse` works on any type that has a .reverse generic *)
```

### Inspecting generics

```
generics []    (* Returns the entire generics table as a record *)
```

---

## Advanced Patterns

### Tail recursion with `tailrec`

Since Square (in the Rust implementation) doesn't have automatic
tail-call optimization, the prelude provides a `tailrec` combinator
that uses `throw`/`catch` and `forever` to simulate TCO:

```
def tailrec
    let {recur: fun x -> throw {Recur: x}}
    fun f -> fun arg -> [
        try
            forever fun [] -> [
            try throw {Exit: f (recur, arg)}
            catch {Recur} -> := arg Recur
        ]
        catch {Exit} ->
            Exit
        ];
```

Usage:

```
def countdown fun start ->
    let {
        aux: tailrec fun (aux', arg) ->
            if > arg 0
            then [println arg; aux' [- arg 1]]
            else println "blastoff!"
    }
    aux start;

countdown 1000;    (* works without stack overflow *)
```

The function passed to `tailrec` receives a pair `(recur, arg)`.
Calling `recur` with a new argument loops back without growing the stack.

### Creating modules

Use the `module` helper (which tags a record with `.module`):

```
def MyLib module {
  greet: fun name -> println [join "" ("Hello, ", name, "!")],
  version: "1.0",
};

MyLib.greet "world";      (* Hello, world! *)
println MyLib.version;     (* 1.0 *)
```

### Loading files

```
load "mylib.sq";
```

This evaluates the file in the current environment, making its
definitions available.

---

## Quick Reference

```
(* Define a variable *)
def x 42;

(* Define a function *)
def double fun x -> * 2 x;

(* Call a function — prefix notation *)
println [double 21];

(* Lists *)
def xs (1, 2, 3, 4, 5);
println [string [map xs [fun x -> * x x]]];    (* (1, 4, 9, 16, 25) *)

(* Records *)
def person {name: "Alice", age: 30};
println person.name;                            (* prints record, then accesses .name on result *)
println [person .name];                         (* Alice *)

(* Pattern matching *)
def fst fun (a, _) -> a;
def snd fun (_, b) -> b;

(* Error handling *)
try [/ 1 0]
catch e -> println "caught an error";

(* Local bindings *)
let {x: 10, y: 20} + x y;    (* 30 *)

(* Branching *)
if > x 0 then "pos" elif = x 0 then "zero" else "neg";

(* Loops via forever + throw *)
def find_first fun xs -> fun pred ->
  let {i: ref 0}
  try forever fun [] ->
    if pred [xs [@ i]]
    then throw {Found: @ i}
    else @= i [+ [@ i] 1]
  catch {Found} -> Found;
```
