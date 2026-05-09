# FUN Language Interpreter using the SECD Machine

## Overview

This repository contains an implementation of an interpreter for **FUN**, a small core functional programming language, built in Haskell. The interpreter is based on the **SECD machine** (Stack, Environment, Control, Dump), a classic abstract machine for evaluating functional programs.

The project was developed as part of a coursework assignment in the Master in Computer Science program at the Faculty of Sciences of the University of Porto.

## Features

- Lambda calculus–based core language
- Call-by-value (strict) evaluation
- Support for:
  - Integer constants
  - Lambda abstractions and function application
  - Let bindings
  - Recursive functions (`fix`)
  - Conditional expressions (`ifzero`)
  - Arithmetic operations (`+`, `-`, `*`)
  - Comparison operations (`>`, `>=`, `==`)
  - Boolean operations (`&&`, `not`)
- Compilation of high-level terms into SECD instructions
- Execution via a full SECD abstract machine

## Language Definition

The FUN language is defined as an algebraic data type in Haskell:

```haskell
data Term
  = Var Ident
  | Lambda Ident Term
  | App Term Term
  | Const Int
  | IfZero Term Term Term
  | Let Ident Term Term
  | Fix Term
  | Add Term Term
  | Sub Term Term
  | Mul Term Term
  | Greater Term Term
  | GreaterEq Term Term
  | Equal Term Term
  | And Term Term
  | Not Term
```

A parser and lexer were implemented using Happy to allow writing programs in a readable syntax.

## Compilation to SECD

Programs written in FUN are compiled into a sequence of SECD instructions. The compilation process:

- Uses a symbol table to map variables to environment positions
- Recursively compiles subterms
- Emits instructions corresponding to each construct

### Instruction Set

```haskell
data Instr
  = LD Int
  | LDC Int
  | LDF Code
  | LDRF Code
  | AP
  | RTN
  | SEL Code Code
  | JOIN
  | ADD | SUB | MUL
  | AND | EQL | GTH | GTE
  | NOT
  | HALT
```

Key ideas:

- Variables are accessed by index (`LD`)
- Functions are compiled into closures (`LDF`, `LDRF`)
- Control flow uses `SEL` and `JOIN`
- Operations assume arguments are already evaluated

## SECD Machine Architecture

The SECD machine is implemented as a 5-tuple:

```haskell
type SECD = (Stack, Env, Code, Dump, Store)
```

### Components

- **Stack**: holds intermediate values
- **Environment**: maps variable positions to values
- **Control**: list of instructions to execute
- **Dump**: stores previous states during function calls and branching
- **Store**: maps addresses to closures

### Values

```haskell
data Value
  = Int Int
  | Addr Addr
```

Closures are stored in memory and referenced by address.

## Execution Model

Execution proceeds step-by-step using a transition function:

- Each instruction transforms the machine state
- Arithmetic and logical instructions consume stack values
- Function application:
  - Looks up closures in memory
  - Extends the environment
  - Saves current state in the dump
- Return (`RTN`) restores previous state
- Branching uses `SEL` and `JOIN`

Evaluation continues until no instructions remain. The final result is the top of the stack.

## Running Programs

The typical execution pipeline:

1. Parse input into a `Term`
2. Compile the term into SECD code
3. Initialize the machine state
4. Execute until completion
5. Return the resulting value

A simple REPL can be used to test programs interactively.

## Example Constructs

Examples of supported expressions:

- Function definition:
  ```
  (\x -> x + 1)
  ```

- Function application:
  ```
  (\x -> x + 1) 5
  ```

- Let binding:
  ```
  let x = 5 in x * 2
  ```

- Recursion:
  ```
  fix (\f -> \n -> if n == 0 then 1 else n * f (n - 1))
  ```

## References

- P. J. Landin, "The Mechanical Evaluation of Expressions", 1964
- Peter Henderson, *Functional Programming: Application and Implementation*, 1980
- Happy Parser Generator for Haskell