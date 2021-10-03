# North

## Index

- Introduction
  - Syntax
  - Semantic
  - References
- Special Forms
- Intrinsics
- Macros

## Introduction

### Syntax

### Semantics

- Point Free Programming
- Combinators
- Function Composition
- First Class Functions
- Pure Functions
- Algebraic Data Types

### References

- Forth: http://www.forth.org/
- Haskell: https://www.haskell.org/
- APL/BQN: https://mlochbaum.github.io/BQN/
- WebAssembly (for typing stack-based languages): https://binji.github.io/posts/webassembly-type-checking/
- Porth (for introducing me to this idea): https://github.com/tsoding/porth

## Special Forms

```forth
# In file `std.north`
,macro write
    1 1 syscall3
.macro
```

```forth
,include
    "std.north"
.include

"Hello World!" io write
```

## Intrinsics

## Macros
