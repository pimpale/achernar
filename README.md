# Achernar

Achernar is a *WIP* multi paradigm programming language focusing on:

* **Minimalism**: Achernar strives to be orthagonal. Language features are simple to understand, independent, and highly composable.
* **Versatility**: Achernar is designed to be 100% compatible with any build system or project setup. The compiler and standard library have no dependencies, and can easily be dropped into any project
* **Safety**: Achernar is strongly typed, and intends to support Ada style contracts (Feature WIP). In the future it aims to focus on static analysis and abstract interpretation.

### Comparison with other languages

Note: Features marked with WIP aren't part of the language yet, but will be implemented soon.

The goal of making a minimalistic low level C based language is nothing new, here are a few other great projects:


| Language | Implemented in | No. Keywords | LOC of Grammar File | LOC Compiler |  Compiled | Interpreted | Macros | Pattern Matching | Closures | Garbage Collection | Configurable Allocator | Sum, Product Types |  Github Stars |
|  ---  i  |  ---           |  ---         |  ---                |  ---         |   ---     |  ---        |   ---  |             ---  |  ---     |  ---               |   ---                  |  ---               |  ---          |
| C (gcc) | 301 | 283 | 290 | 286 | 289 | 285 | 287 | 287 | 272 | 276 | 269 | 254 |
| Zig | 301 | 283 | 290 | 286 | 289 | 285 | 287 | 287 | 272 | 276 | 269 | 254 |
| Nim | 301 | 283 | 290 | 286 | 289 | 285 | 287 | 287 | 272 | 276 | 269 | 254 |
| Kit | 301 | 283 | 290 | 286 | 289 | 285 | 287 | 287 | 272 | 276 | 269 | 254 |
| Achernar | 301 | 283 | 290 | 286 | 289 | 285 | 287 | 287 | 272 | 276 | 269 | 254 |

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

Achernar is a star in the constellation Eridanus. I chose the name because it sounds cool. 
