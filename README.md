# Achernar

Achernar is a *WIP* multi paradigm programming language focusing on:

* **Minimalism**: Achernar strives to be orthagonal. Language features are simple to understand, independent, and highly composable.
* **Versatility**: Achernar is designed to be 100% compatible with any build system or project setup. The compiler and standard library have no dependencies, and can easily be dropped into any project
* **Safety**: Achernar is strongly typed, and intends to support Ada style contracts (Feature WIP). In the future it aims to focus on static analysis and abstract interpretation.

### Comparison with other languages

Note: Features marked with WIP aren't part of the language yet, but will be implemented soon.

The goal of making a minimalistic low level C based language is nothing new, several other projects such as:
* Zig
* Nim
* Kit

have all tried. I have compiled a table of some heuristics to compare between them

(All stats as of July 2020)

Note: These are simply heuristics to measure the complexity of a language, and don't necessarily do a good job

| Language  | Implemented in | No. Keywords | No. Operators | LOC of Grammar File | LOC Compiler |  Compiled | Interpreted | Macros | Pattern Matching | Closures | Garbage Collection | Configurable Allocator | Sum, Product Types |  Github Stars |
|  ---      |  ---           |  ---         |  ---                |  ---         |   ---     |  ---        |   ---  |             ---  |  ---     |  ---               |   ---                  |  ---               |  ---          |
| C11 (gcc) | C++            | 32  | 290 | 286 | 289 | 285 | 287 | 287 | 272 | 276 | 269 | 254 |
| Zig       | C++ (Zig WIP)  | 46 | 290 | 286 | 289 | 285 | 287 | 287 | 272 | 276 | 269 | 254 |
| Nim       | 301            | 65 | 290 | 286 | 289 | 285 | 287 | 287 | 272 | 276 | 269 | 254 |
| Kit       | Haskell        |  | 290 | 286 | 289 | 285 | 287 | 287 | 272 | 276 | 269 | 254 |
| Achernar  | C11            | 283 | 290 | 286 | 289 | 285 | 287 | 287 | 272 | 276 | 269 | 254 |

### Current Status

Complete:
- [x] Portable C Standard Library
- [x] Lexer
- [ ] Macros
- [x] Recursive Descent Parser
- [x] Lower AST to HIR
- [x] Name Resolution
- [x] Namespaces
- [ ] Lower HIR to MIR
- [ ] LLVM code generation
- [ ] Bootstrap Compiler
- [ ] Rewrite standard library in Achernar

### Naming

Achernar is a star in the constellation Eridanus. I chose the name because it sounds cool 


