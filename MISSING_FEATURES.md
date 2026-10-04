# OxAlc Missing Features and Incomplete Areas

This document captures the main gaps identified during a full review of the type-checking and compiler code, especially in [OxAlc.Service/Checker.fs](./OxAlc.Service/Checker.fs), alongside the broader compiler pipeline in [OxAlc.Service/Compiler.fs](./OxAlc.Service/Compiler.fs) and the project design in [README.md](./README.md).

## Executive Summary

The project is clearly still a prototype and work in progress. The language design is ambitious, but the implementation is incomplete in several critical areas:

- type inference is not general or robust
- function application validation is limited
- match-expression typing is brittle
- record/struct typing is heuristic-based
- operator support is partial
- lambda compilation is not yet production-ready
- error handling is too rough for a compiler pipeline

The codebase shows a promising prototype, but it is not yet a complete or reliable typed language implementation.

---

## 1. Lambda target is not complete

The root project documentation states that the lambda backend is not yet up to date. This is a critical indicator that the core compiler target is unfinished.

Evidence:

- [README.md](./README.md) explicitly says: "Lambda target is not yet up-to-date, currently exploring adding STLC"
- [OxAlc.Service/Compiler.fs](./OxAlc.Service/Compiler.fs) contains multiple `failwith "Not Implemented"` and `failwith "Not Implemented or Not Compilable"` branches.

Implications:

- The pipeline does not reliably compile all supported statements.
- Some valid language forms cannot be lowered to Lambda Calculus.
- The language is not yet safe to describe as a fully working compiler target.

Missing or incomplete work:

- complete lambda lowering for all arithmetic and boolean constructs
- finish operator lowering for unsupported cases
- verify generated lambda terms against real examples
- unify the lambda backend with the type checker semantics

---

## 2. Type inference is only partial and not fully general

The type checker in [OxAlc.Service/Checker.fs](./OxAlc.Service/Checker.fs) performs a fair amount of type checking, but it does not implement a general inference strategy comparable to Hindley–Milner or a modern ML-style type engine.

Current behavior:

- it compares types using direct equality in multiple code paths
- it resolves aliases only in a narrow way
- it supports some exact matches but not full polymorphic inference
- it relies on many special-case rules instead of a generic algorithm

Problems:

- no generalized type variables
- no proper polymorphic function support
- no robust principal type derivation
- no full inference across nested functions, applications, and containers

This is a major gap if the language is meant to feel like a typed functional language.

Missing or incomplete work:

- infer polymorphic function types
- infer fresh type variables for expressions where types are implicit
- generalize/infer over nested lambda expressions
- support type variables in application and binding contexts

---

## 3. Function application validation is too narrow

The `Application` case in the type checker is one of the most obvious under-implemented parts.

Current implementation issues:

- exact match only in many paths
- alias checking is narrow
- union compatibility is handled through a small special case
- custom operator and function application logic is brittle

Example weaknesses:

- no true type unification of argument and parameter types
- limited support for partial application
- limited support for higher-order function semantics
- no systematic handling of generic argument/return inference

Missing or incomplete work:

- full function-call type checking against parameter lists
- proper partial application support
- robust treatment of higher-order functions
- built-in inference for alias/union types in application scenarios

---

## 4. Match expressions are fragile and incomplete

The `Match` logic is a key weak point in the type checker.

Current issues:

- it reconstructs types from patterns in a loose way
- it relies on `flattenIfUnionType` and union flattening heuristics
- it falls back to broad `type mismatch` errors frequently
- it does not appear to check exhaustiveness or invalid pattern coverage

This is a severe limitation because pattern matching is a core feature in a typed ML-like language.

Missing or incomplete work:

- exhaustiveness checking for all constructors/patterns
- invalid-pattern detection
- proper union decomposition for multiple alternatives
- clear return-type synthesis from all branches
- stronger support for typed case identifiers and destructuring

---

## 5. Record and struct typing is heuristic-based rather than solidly modelled

The `Value Record` branch in [OxAlc.Service/Checker.fs](./OxAlc.Service/Checker.fs) tries to identify matching struct definitions by scanning all structs in context and filtering candidates.

Problems:

- it uses a broad `Map.values ctx.Types` scan
- it does not validate record shape consistently
- field typing depends on heuristics and exact matching
- error reporting is weak
- nested or custom record types are not handled systematically

This is not a strong structural typing system.

Missing or incomplete work:

- proper field existence validation
- clear mismatched-field diagnostics
- support for nested record values
- type aliasing for record fields without fragile special cases
- better matching of record literals to declared record types

---

## 6. Type alias resolution is unsafe and inconsistent

Several areas use recursive logic such as `actualTypeOf`, `isTypeMatch`, and alias lookups via `Map.tryFind` / `Map.find`.

Problems:

- missing key handling is not always robust
- alias chains can be unresolved or malformed
- recursive type aliases are not clearly validated
- alias resolution is mixed with actual type matching in a way that can produce partial or inconsistent behavior

Missing or incomplete work:

- detect circular alias definitions
- provide a safe alias expansion algorithm
- centralize alias resolution in one reusable path
- return proper diagnostics instead of throwing or failing unexpectedly

---

## 7. Operator support is partial and inconsistent

The binary operator logic in [OxAlc.Service/Checker.fs](./OxAlc.Service/Checker.fs) supports only a subset of operations and uses a catch-all error branch for unsupported ones.

Current issues:

- some operators are implemented only for numbers or bools
- `Dot` is ad hoc and not generalized
- `Not` is not properly modeled for all valid contexts
- custom binary operators are checked with a narrow exact type pattern
- comparison semantics are not strong across all types

Missing or incomplete work:

- complete typing for all built-in operators
- consistent behavior across numbers, words, bools, and unions
- proper custom operator typing rules
- cleaner support for list/record operations

---

## 8. Dot member access is under-specified

The `Dot` case tries to handle multiple scenarios:

- access through a type alias
- access through a struct value
- access through an intersection by index

This is not a principled implementation of field/member access.

Problems:

- no formal object model
- no generalized member lookup algorithm
- access by numeric index is too ad hoc
- not all record-like values are treated consistently

Missing or incomplete work:

- structured field lookup for records and structs
- safe access by identifier or path
- better support for tuple and intersection indexing
- explicit validation for missing members

---

## 9. Tuple and list typing are simplistic

The type system defines `Tuple` and `List`, but the implementation does not treat them as strong typed values.

Current behavior:

- `List` is treated as a single `Atom "list"`
- `Tuple` reduces to an `Intersection` of element types, which is not a robust representation for position-sensitive values

This means the language cannot model list element constraints or positional tuple semantics reliably.

Missing or incomplete work:

- parameterized list types such as `list<number>` or equivalent
- tuple typing with clear position-sensitive semantics
- list operations with typed element validation
- compatible support in both checker and compiler

---

## 10. Error handling is not compiler-grade

The type checker and compiler use `failwith` in several places to signal unhandled cases.

Examples:

- `failwith "Invalid type definition"`
- `failwith "Unreachable code"`
- compiler branches with `failwith "Not Implemented"`

This is acceptable for a prototype, but not acceptable for a compiler that is expected to produce helpful diagnostics.

Missing or incomplete work:

- replace fatal exceptions with typed error results
- report source-contextual diagnostics
- surface unsupported constructs clearly instead of crashing
- distinguish internal compiler bugs from user-program errors

---

## 11. Context/scope handling is weak

`Bind`, `Compound`, and `Context` handling are implemented but not strongly validated.

Problems:

- shadowing rules are not enforced clearly
- symbol additions may be too permissive
- no clear distinction between local and global binding states in many cases
- scope leaks are possible in some nested structures

Missing or incomplete work:

- formal environment-scoping rules
- safe shadowing checks
- sequential binding validation and usage visibility
- strong validation of imported context data

---

## 12. The README examples are ahead of the implementation

The examples in [README.md](./README.md) describe a rich typed language with:

- typed records
- includes and modules
- pattern matching
- typed functions
- operator style syntax
- lambda and JS output

That level of functionality is not yet consistently backed by the code in the checker and compiler.

This is not necessarily a bug, but it is an important status warning: the project is more aspirational than fully implemented.

---

## Priority Ranking

### Highest priority

1. Lambda backend completion
2. General type inference and unification
3. Match expression correctness
4. Function application validation
5. Record/struct typing reliability

### High priority

1. Operator typing completeness
2. Alias resolution safety
3. Error diagnostics and elimination of `failwith` crashes
4. Tuple/list typing completeness

### Medium priority

1. Scope and symbol-table validation
2. Dot/member access model
3. Standardization of nested type semantics

---

## Recommended next actions

1. Treat the language as a prototype rather than a production-ready compiler.
2. Prioritize correctness of the type system before adding more syntax.
3. Replace ad hoc checks with a clearer unification/inference model.
4. Finish the lambda lowering pipeline before trying to scale features.
5. Add test coverage for the examples in [README.md](./README.md) and ensure they pass before adopting more syntax.

---

## Final assessment

The project is promising, but it is not yet complete enough to be described as a fully functional typed language implementation. The largest missing work is not a single missing feature; it is the fact that the type system, compiler pipeline, and language semantics are still too incomplete and brittle to support the design described in the documentation.
