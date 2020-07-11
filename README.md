# Achernar

Achernar is a *WIP* multi paradigm programming language focusing on:

* **Minimalism**: Achernar strives to be orthagonal. Language features are simple to understand, independent, and highly composable.
* **Versatility**: Achernar is designed to be 100% compatible with any build system or project setup. The compiler and standard library have no dependencies, and can easily be dropped into any project
* **Safety**: Achernar is strongly typed, and intends to support Ada style contracts. In the future it aims to focus on static analysis and abstract interpretation.

### Motivating factors

There are several design goals for Achernar:

* Fully public domain source code
  * Achernar is fully public domain, which means that you are free to embed it anywhere, without attribution.
* 1:1 Correspondance with AST. 
  * Means that the comments, macros, and code should all be representable from the AST with no data loss. 
  * This eases the implementation of code transforming tools such as formatters, static analyzers, and machine learning tools
* Comment integration with code
  * Achernar defines which expression, statement, type, or macro a comment applies to. 
  * Enables code formatters will be able to better respect comment placement and formatting
  * It can also be used to generate more specific documentation and 
* No Redundancy
  * Unify `if`, `switch`, and the ternary operator (`?`) into `match`
  * Unify `while(...)`,`do { } while(...)`, and `for(...)` into `loop`
    * We can use the iterator's `foreach` callback function to handle looping over a collection
* No semicolons
  * Semicolons are made redundant if the grammar is unambiguous
  * Achernar takes Lua's approach by prohibiting language features that would make it impossible to distinguish the end of a statement from the beginning of another.
 
 
### Build & Installation Instructions

To install Achernar, git clone the repository and run make in `bootstrap1`. Requires `clang` or `gcc` to compile.

```bash
$ cd bootstrap1
$ make
```

### Run Instructions

Once you've built Achernar, you should be able to find the binary in the `obj` folder

```bash
$ ./obj/achernar
```

At the moment, it only serves as a REPL, but it will change as more features are introduced.
 
### Code examples

Here are some examples of what Achernar code looks like. 

#### Hello World

```
_sysembed` com_io `

com::io:println("hello")

```

The `_sysembed` is an intrinsic macro, and enables embedding files inside this one. It includes the namespace com::io. Functions from this namespace are utilized in the next line to print a string. Note that semicolons are not necessary.

#### Declaring variables and functions
```
# you can comment a statement
val my_string := "string"

# type inference is available, but you can additionally annotate variables by placing a colon after their name
val my_int:i32 := 500


# Achernar has first class functions. 
val double_this := fn(example_name:i32):i32 => example_name*2

#{ 
  Block Comments
  Are Also Possible
}#

double_this(my_int)
```

All functions are defined through the syntax shown above. This is because functions are a kind of value themselves.

#### Fizzbuzz 
```
_sysembed` com_io `
_sysembed` com_alloc `
_sysembed` com_format `

val allocator := com::alloc::system();

val i := 0
# we name this loop " 'x ". The tick ahead of the x means that it is a label for the loop. It introduces a new scope 'x
loop 'x {
    # here we utilize tuples (aka product types) to unite i % 5 and i % 3 into one value that we match on
    # the "," operator unites two values into a tuple
    i % 3, i % 5  match {
    # this pattern matches if both i % 3 and i % 5 evaluate to zero
    # this  pattern deconstructs the tuple, using the "," operator, which in a pattern context splits the tuple
    pat ==0, ==0  => "fizzbuzz"
    # patterns are evaluated in order, so this pattern would be checked next
    # it will run when the counter is divisible by 3
    # The "_" pattern will match any operator
    pat ==0, _    => "fizz"
    # this will run when the counter is divisible by 5
    pat _  , ==0  => "buzz"
    # otherwise, we convert the int to a string
    # because this process requires the allocation of memory for a variable length string, we also pass the allocator object
    # when we free this allocator, all memory allocated with it will be released
    pat _ => com::format::int_to_str(i, allocator)
  } # The Arrow operator subsitutes the left hand operand into the first argument of the right hand side function 
    ->com_io_printf()
    
  # increment  counter
  i += 1
  # this match statement decides whether to terminate the loop
  i match {
    # we return "nil" from the scope 'x
    pat > 100 => ret 'x nil
    # do nothing if i is less than or equal to 100
    pat _ => nil
  }
}

# free all memory
allocator->com::alloc::free()

```
This example demonstrates how to branch, loop, and allocate memory.


### Comparison with other languages

The goal of making a minimalistic low level C based language is nothing new, several other projects such as: Zig, Nim, Kit, and V have all tried. I have compiled a table of some heuristics to compare between them:

| Language    | Implemented in | No. Keywords | No. Operators | LOC of Grammar File | Compiled   | Interpreted | Macros                  | Generics | Pattern Matching | Operator Overloading | Closures | Garbage Collection | Configurable Allocator | Algebraic Data Types | Github Stars  |
|  ---        |  ---           |  ---         |  ---          |  ---                |  ---       |   ---       |  ---                    |  ---     |  ---             |   ---                |  ---     |  ---               |  ---                   |  ---                 |  ---          |
| C11 (clang) | C++            | 32           | 48            | 172 (EBNF)          | Yes, LLVM  | No          | Yes (Text Substitution) | No       | No               | No                   | No       | No                 | No (stdlib uses malloc)| No                   | 5300 (mirror) |
| Zig         | C++ (Zig WIP)  | 46           | 48            | 376 (PEG)           | Yes, LLVM  | No          | No                      | Yes      | No               | No                   | No       | No                 | Yes                    | Yes                  | 6000          |
| Nim         | Nim            | 65           | 40            | 206 (custom)        | Yes, C, JS | Yes,        | Yes (Ast Substitution)  | Yes      | Yes              | Yes                  | Yes      | Yes                | No                     | Yes                  | 9600          |
| Kit         | Haskell        | 42           | 36            | 232 (Haskell)       | Yes, C     | Yes         | Yes (Term Rewriting)    | Yes      | Yes              | Yes                  | No       | Yes                | No                     | Yes                  | 950           |
| V           | V              | 29           | 38            | N/A (couldn't find) | Yes, C     | No          | No                      | Yes      | Yes              | Yes                  | No       | No                 | No (depends on C)      | Yes                  | 17900         |
| Achernar    | C11            | 17           | 40            | 118 (EBNF)          | Yes, LLVM  | Yes         | Yes (Term Rewriting)    | Yes      | Yes              | No                   | Yes      | No                 | Yes                    | Yes                  | 0             |

Note: Since Achernar isn't complete yet, many features marked on the table for Achernar are still WIP.

### Current Status

The compiler is currently in a heavy state of development. The follwing is a chronological list in which features are likely to be implemented. Finished + Finalized features have been marked with a checkbox. Unchecked features are all being worked on, but not complete

- [x] Portable Common Library (comlib)
  - [x] Json Parsing Library
  - [x] Vectors, Queues, Hashmaps, and other data structures
  - [x] Custom Allocator
- [x] Lexer
- [x] Recursive Descent Parser
- [x] Lower AST to HIR
- [x] Name Resolution
- [x] Namespaces
- [ ] Term Rewriting Macros
- [ ] Lower HIR to MIR
- [ ] LLVM code generation
- [ ] Rewrite comlib in Achernar
- [ ] Bootstrap Compiler
- [ ] Generics + Type Inference
- [ ] Functors + Closures
- [ ] LSP Server
- [ ] Abstract Interpretation engine

### Naming

Achernar is a star in the constellation Eridanus. I chose the name because it sounds cool. 😎

