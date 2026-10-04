# OxAlc Project Overview

## 1. What this project is

OxAlc is a small custom language and compiler project that blends:

- a domain-specific language for typed expressions and records
- a parser for that language
- a type checker for algebraic/structural types
- a compiler that can target Lambda Calculus and JavaScript
- a small interpreter for evaluating Lambda Calculus expressions

The project is clearly experimental and still described as a work in progress in the root README. The design aims to support a ML-like syntax with lambda-based execution and type inference, while also producing JavaScript output for easier runtime evaluation.

---

## 2. High-level purpose

The main idea is: write a language that looks like a typed functional language, parse it, infer enough types to validate it, then either:

1. translate it into Lambda Calculus, or
2. emit JavaScript code.

This is visible in the project structure:

- `OxAlc.Service` contains the compiler, parser, interpreter, and type system
- `OxAlc.Console` is the command-line entrypoint that reads input and executes it
- examples under `OxAlc.Console/Examples` show the DSL in use

The README examples show a language with typed values, function literals, records, includes, and operator-style syntax.

---

## 3. Main architectural pipeline

The system follows a fairly standard compiler pipeline, although it is hand-rolled rather than built on a formal compiler framework.

### Pipeline

1. Source text is read from a file or terminal input
2. The parser converts text into an internal AST
3. The type checker validates or infers types
4. The compiler emits a target representation
5. The interpreter evaluates lambda terms when the Lambda backend is used

### Core flow in code

The entrypoint in `OxAlc.Console/Program.fs` reads user input and decides whether to run a Lambda or JavaScript-style pipeline. It calls `transpile` from the compiler module and then interprets or prints the result.

---

## 4. File-by-file architecture and why each piece exists

### Root project files

#### README.md
Why it matters:
- It is the project’s intended specification and usage guide.
- It shows the language syntax as it was meant to feel to the programmer.
- It explains the intended target behavior: typed expressions, records, includes, and lambda/JS compilation.

What it demonstrates:
- binding values with `let`
- function definitions with arrow syntax
- record and module-style organization
- custom operators and higher-order functions
- compiled output examples in both lambda and JavaScript forms

#### Lmbda.sln and LambdaProject.sln
Why they matter:
- These are solution definitions that define how the code is organized and built.
- The naming inconsistency suggests the project evolved over time, but both are references to the same conceptual project.

#### OxAlc.Console/Examples
Why they matter:
- These sample `.oxalc` files are executable examples of the language.
- They serve as regression-style tests of syntax and semantics while also acting as documentation.

Examples include:
- `calculator.oxalc`
- `constants.oxalc`
- `functions.oxalc`
- `operators.oxalc`
- `programs1.oxalc` and `programs2.oxalc`

---

### OxAlc.Service/Definitions.fs
This is the semantic heart of the project. It defines the internal representation used everywhere else.

#### Major types and why they exist

##### Expression
This models the purely symbolic lambda-like syntax used by the interpreter and the compiler.

- `Term of string` = a variable or identifier
- `Applicative of Expression * Expression` = function application
- `Lambda of Expression * Expression` = a lambda abstraction

Why it matters:
- This is the core lambda AST used after lowering a source-language program.
- The compiler converts parsed source into this form for lambda evaluation.

##### Envirement and Thunk
- `Envirement` = a list of name-to-expression bindings
- `Thunk` = just an alias for `Expression`

Why they matter:
- These represent the runtime environment and delayed evaluation concept used in functional programming.

##### Type
This is the project’s type algebra.

- `Atom of string` = primitive type or named type
- `Union of Type list` = type alternatives
- `Intersection of Type list` = required combined types
- `Exponent of Type * Type` = function type, like `A -> B`
- `Struct of Map<Statement, Type>` = record-like typed structure

Why it matters:
- The parser and type checker build this model when they process source programs.
- This allows the language to reason about function signatures and record layouts.

The `ToString()` override is important because it renders types in a readable form for error messages and emitted code.

##### TypingContext
This holds the current symbol table and type aliases during type resolution.

- `Symbols: Map<string, Type>` stores bound names and inferred types
- `Types: Map<string, Type>` stores user-defined or alias types
- `PrimitivesMap` seeds the language with built-ins like `word`, `bool`, `number`, `type`

Why it matters:
- Type checking is context-sensitive; this structure is what makes it possible to know what a name means in scope.

##### Statement
This is the main source-level AST.

- `Value` = literal values or literal containers
- `Function` = a function definition with parameters and body
- `Application` = calling a function with argument list
- `Identifier` = a symbol reference
- `Bind` = a `let`-style binding with type info, expression, and continuation
- `Unary` = unary operator application
- `Binary` = infix operator application
- `Branch` = `if ... then ... else ...`
- `Match` = pattern matching
- `Compound` = grouped bindings like `where ... and ...`
- `Context` = region with imported files and program body

Why it matters:
- This is the AST the parser builds from source code.
- The compiler and type checker both traverse this representation recursively.

##### Literal
These are expression values and typed object literals.

- `Bool`, `Variable`, `String`, `List`, `Record`, `Tuple`, `TypeDefinition`

Why it matters:
- The language supports rich literal grammar, and this representation is the point where those grammar nodes turn into typed semantic values.

##### Operation
A set of supported operators and infix forms.

- `Cons`, `Add`, `Subs`, `Div`, `Mult`, `Exp`, `Or`, `And`, `Eq`, `Neq`, `Lt`, `Not`, `Xor`, `Gt`, `YComb`, `Dot`, `Custom of string`

`Operation.toOp` converts token sequences like `+`, `-`, `=`, or custom names into internal operator values.

Why it matters:
- This is the unification layer between source syntax and runtime semantics.

##### Backend
Enum for compilation targets:
- `LCR`, `LLVM`, `MSIL`, `JS`

`Backend.Parse` turns strings into backend values.

Why it matters:
- The compiler chooses the target pipeline based on this enum.

##### Result
This wraps the final output types of different backends.

- `LCRR` for lambda output
- `JSR` for JS output
- `LLVMR` for LLVM output
- `MSILR` for MSIL output

Why it matters:
- It standardizes the compiler output contract.

---

### OxAlc.Service/Parsec.fs
This file is the custom parser framework. It is not a language parser by itself; it is the reusable foundation that makes parser combinators possible.

#### Important functions and why they exist

##### `toString`
Converts a `char list` to a string by mapping each char to its string form and concatenating them.

Why it matters:
- This is used heavily in parser code to convert sequences of characters into identifiers and tokens.

##### `Position` and state tracking
- `Position = Cursor of (int * int)` stores line and column.
- `initial Position` sets the cursor to the start of input.
- `incrCol` and `incrLin` advance position as parser consumes input.

Why it matters:
- Good error messages and parser feedback require accurate source positions.

##### `State`
- `State = Input of (char list[] * Position)` stores the current remaining input and cursor.
- `fromStr` converts a string to parser state.
- `currentLine` and `next` move through the source text.

Why it matters:
- The parser is stateful; every parse step needs to know what remains to be consumed.

##### Parser type and monad
- `Parser<'a>` wraps a function from `State` to a result.
- `ResultError = string * ErrorMessage * ParserPosition option`
- `run word p` executes the parser on input.
- `bind` composes parsers sequentially.
- `give` creates a parser that always succeeds with a value.
- `ParserMonad` implements the F# computation expression syntax used across the project.

Why it matters:
- This is the parser combinator runtime.
- It allows the code to write parser logic in a clean, reusable, compositional form.

##### Primitive combinators
- `satisfy` parses a character matching a predicate
- `expect` parses an exact character
- `anyOf` tries a list of alternatives
- `allOf` parses a full sequence of expected chars
- `choice` tries many parsers and takes the first success
- `sequence` combines parsers in order
- `many n parser` repeatedly parses items
- `option parser` makes a parser optional
- `between` wraps a parser between delimiters
- `separateBy` parses repeated lists separated by a delimiter
- `eof` ensures the parser reaches the end of input

Why it matters:
- These primitives make the source-language grammar possible without a formal parser generator.

---

### OxAlc.Service/Parser.fs
This is the actual source-language parser. It takes a string and turns it into the `Statement` AST from `Definitions.fs`.

#### Core entry point
##### `parseExpr`
This is the top-level parser for expressions.

It composes a large list of parser alternatives:
- `parseLibrary`
- `parseMatch`
- `parseBrancher`
- `parseInclude`
- `parseLet`
- `parseCompound`
- `parseFunction`
- `parseOperation`
- `parseValue`
- `parseBinary`
- `parseUnary`
- `parseIdentifier`

Why it matters:
- This is the language entry point; most source code flows into this parser.

#### Parsing type syntax
##### `parseType`
This recursively parses type expressions including:
- base atoms
- union `|`
- intersection `&`
- exponent `->`
- struct/object types `{ ... }`

Why it matters:
- The language is strongly typed, and type syntax must be parsed before checking and compiling.

Key helper methods:
- `parseUnit` parses primitive type names
- `parseSum` parses unions
- `parseMult` parses intersections
- `parseExp` parses arrow types
- `parseStruct` parses record-like type declarations

#### Parsing values and literals
##### `parseValue`
This parses literal values like:
- booleans
- numbers
- tuples
- lists
- record literals
- type definitions

Why it matters:
- Values are the leaves of the program AST.
- Without this stage, the language could not build expressions from concrete syntax.

Important internal parsers:
- `parseSimple` handles `true`, `false`, and number literals
- `parseTuple` parses comma-separated tuples
- `parseList` parses list literals
- `parseString` parses string literals
- `parseTypeDefinition` allows types to be treated as values when needed
- `parseRecord` parses record syntax using `parseLibrary`

#### Binding and program structure
##### `parseLet`
This parses construct like:
- `let x : type = value in ... end`

It returns a `Bind` node with:
- identifier
- suggested type
- bound expression
- continuation expression

Why it matters:
- This is the primary binding construct in the language, similar to ML or F#.

##### `parseCompound`
This handles compound expression forms such as:
- `where ... and ...`

It inlines several variable bindings before continuing evaluation.

Why it matters:
- It supports local scoping and structured expression chaining.

##### `parseInclude`
Parses constructs like:
- `include [file1, file2] for ...`

It creates a `Context` node that records imported dependencies and the main expression.

Why it matters:
- This is how the language supports separate modules and reusable record libraries.

#### Expression forms
##### `parseBrancher`
Parses `if condition then expr else expr` and turns it into a `Branch` node.

Why it matters:
- Control flow is encoded as expression branching rather than statements.

##### `parseFunction`
Parses lambda-like function syntax such as:
- `(x:number, y:number) => x + y`

It produces a `Function` AST node.

Why it matters:
- Functions are central to this language’s type system and runtime semantics.

##### `parseUnary`
Parses unary operators like:
- `~`, `-`, `Y`, `rec`

Why it matters:
- This supports nontrivial operator semantics and recursive constructs.

##### `parseBinary`
Parses infix expressions like:
- `a + b`
- `x = y`
- `a & b`

Why it matters:
- Most language operations are binary in this DSL.

##### `parseOperation`
This handles application-like syntax where a function or expression is called with arguments in parentheses.

It creates an `Application` node.

Why it matters:
- The language supports calling arbitrary expressions with argument lists.

##### `parseMatch`
Parses `match x with | p => expr | _ => fallback` patterns.

Why it matters:
- This is how the project experiments with pattern-based type narrowing and union matching.

##### `parseIdentifier`
Parses symbols like `x`, `value1`, `add`.

Why it matters:
- Names are the bridge between symbols and the type context.

##### `parseLibrary`
Parses record-like library blocks in `{ ... }` syntax.

It builds pairs like:
- `name : type = value;`

Why it matters:
- This is the main way the language models structured objects and reusable modules.

---

### OxAlc.Service/Checker.fs
This is the static semantics layer. It decides whether a program type-checks and what each expression’s type is.

#### Core utility functions
##### `addSymbol`
Adds a variable name to the symbol table with a type.

##### `addType`
Adds a named type alias to the type environment.

Why these matter:
- Type checking is not just about expressions; it also needs environment mutation as names and types are introduced.

##### `actualTypeOf`
Resolves aliases to their underlying type definitions.

Why it matters:
- This lets the checker follow type names back to their actual definitions.

##### `isTypeMatch`
Checks whether a given type is compatible with another based on a context-aware matching rule.

##### `flattenIfUnionType`
Expands or normalizes union types so type comparisons are easier.

##### `flattenResults`
Flattens a list of `Result`s into one `Result` or early error.

##### `addRange`
Adds a whole set of parameters to the typing context.

Why these matter:
- The type checker is a recursive environment-based system; these helpers maintain consistency across scopes.

#### The central method: `TypeOf`
This is the main function that recursively infers or validates types.

It matches on many AST nodes:

##### `Bind`
Checks if a bound value matches its declared type and updates the context accordingly.

Why it matters:
- `let` is the major declaration unit; this is where scope and typing are established.

##### `Match`
Validates the input expression against pattern return types and computes a result type.

Why it matters:
- This is the type-based logic behind structural and union matching in the DSL.

##### `Branch`
Ensures a condition is boolean and that both branches have compatible types.

Why it matters:
- If-expressions are typed by requiring a boolean condition and a stable branch result type.

##### `Identifier`
Looks up a variable name in the current context.

Why it matters:
- Without this, expressions could never resolve to the right symbol types.

##### `Function`
Builds a function type like `A -> B` from parameter types and return type.

Why it matters:
- This is how the checker encodes higher-order functions.

##### `Application`
Checks whether a function’s input type matches each argument type and propagates the output type.

Why it matters:
- This is the main rule for call validation.

##### `Compound`
Processes grouped bindings and then type-checks the rest of the expression with the new scope.

Why it matters:
- It supports local definitions and the sort of expression chaining the language likes.

##### `Value`
Infers types for primitive literals and aggregates like tuples, records, and lists.

Why it matters:
- Every literal must have a precise type before further checking can succeed.

##### `Binary`
Handles type rules for operators like `+`, `-`, `=`, `and`, `or`, `.` and custom operators.

Why it matters:
- This is where arithmetic, comparison, union logic, and field access are validated.

##### `Context`
Delegates to the program inside imported or grouped contexts.

Why it matters:
- This allows nested modules and imported code to remain type-checked in the correct environment.

---

### OxAlc.Service/Compiler.fs
This is the transformation layer. It converts the AST into a target representation.

#### Main entry point
##### `transpile backend rawInput`
This is the central compiler function.

It does the following:
1. removes comments from the source text
2. parses the source into the AST
3. resolves `include` dependencies
4. injects imported record fields into the current program
5. runs `TypeOf` to make sure the program is semantically valid
6. emits a target representation depending on the backend

Why it matters:
- This is the “compiler controller” for the whole project.

#### Helper methods
##### `curry`
Rewrites multi-parameter functions into nested single-parameter functions.

Why it matters:
- Lambda calculus usually uses curried functions, so this prepares the AST for lambda-based evaluation.

##### `parseExp`
Parses a raw string into the AST using the custom parser.

##### `getTypeOf`
Resolves a symbol’s type from the `TypingContext`.

##### `injectFields`
Takes record fields and injects them into the continuation program as bindings.

Why these matter:
- They allow imported modules to be added into the active execution context while preserving type information.

#### Backend-specific emission
##### Lambda backend
The `LCR` branch builds a lambda representation from the AST by recursively emitting forms such as:
- identifiers
- bindings
- functions
- applications
- branches
- binary operations
- values like booleans and lists

Why it matters:
- This is the “compile to lambda calculus” path.

##### JavaScript backend
The `JS` branch produces JavaScript-ish code strings for functions and object values.

It emits expressions like:
- function objects with typed wrappers
- calls like `(expr).value(arg1)(arg2)`
- nested arrow-like constructs

Why it matters:
- This is the path designed to output executable JavaScript from the language.

The compiler is intentionally not a perfect production compiler; it is a research-oriented translation layer between a custom syntax and different execution models.

---

### OxAlc.Service/Interpreter.fs
This file is the runtime evaluator for lambda expressions.

#### Parsing lambda syntax
##### `parseExpr`
The lambda parser chooses one of:
- `parseAtom`
- `parseApp`
- `parseLambda`

##### `parseAtom`
Reads identifiers like `x`, `y`, `value`.

##### `parseApp`
Parses application nodes like `(f x)`.

##### `parseLambda`
Parses lambda abstractions like:
- `\x.y`
- `λx.y`

Why they matter:
- These functions define the lambda expression grammar in the evaluator.

#### Evaluation logic
##### `evalExpr`
This is the big reduction engine.

It does:
- alpha conversion to avoid conflicting names
- substitution for beta reduction
- recursive reduction until normal form
- expression simplification

The helpers inside it are:
- `convert` = renames bound variables to avoid collisions
- `substitute` = replaces a variable with a value
- `isNormalForm` = checks whether an expression is fully reduced
- `reduce` = performs beta reduction recursively

Why it matters:
- This is the actual runtime engine that makes lambda calculus executable.

##### `toString`
Converts a lambda expression into printable text.

Why it matters:
- This makes debugging and output readable.

##### `parse` and `interpret`
- `parse` wraps the lambda parser for a given string
- `interpret` takes the parsed expression and returns the reduced result

Why they matter:
- They form the public API of the evaluator.

---

### OxAlc.Service/Library.fs
This is a convenience module around the service logic.

#### `transpile`
Wraps `OxalcCompiler.transpile` and formats the result as a string.

#### `interpret`
Uses `Interpreter.parse`, then checks if the result is a lambda result and prints it.

#### `decompile`
Declared as a placeholder for uncompile-style behavior but not fully implemented.

#### `parse`
A public parser selector that routes calls by mode:
- `Lambda` -> use `Interpreter.parse`
- `Oxalc` -> use `OxalcParser.parse`

Why it matters:
- This is the higher-level API façade for the service library.

---

### OxAlc.Console/Program.fs
This is the user-facing application layer.

#### `parseArguments`
Reads raw CLI arguments and builds a mapping of option names to values.

It applies default values:
- `--mode` defaults to `Terminal`
- `--back` defaults to `LCR`

Why it matters:
- The program needs to know whether to read from a file or terminal and what backend to use.

#### `Mode`
An enum-like type:
- `File of string`
- `Terminal`
- `Lambda`

`Mode.fromString` converts a string to this mode.

Why it matters:
- It allows the CLI to model different behaviors in a single type.

#### `REPL`
This is the central application loop.

It defines:
- `execute mode backend`: the runtime harness
- `read`: chooses how source is obtained (file input or terminal input)
- `eval`: decides which operation to run based on mode
- `print`: prints the result
- `loop`: recurses over the session

Why it matters:
- This is the interactive development environment of the project.

This file is what turns the compiler library into a usable user program.

---

## 5. Language ideas actually present in the code

The project is trying to model a language with a fairly rich functional expression style.

### a) Typed values and literals

The parser can read:

- booleans: `true`, `false`
- numbers: `0`, `1`, `23`
- strings
- lists
- records
- tuples
- typed definitions

The type checker knows primitive types such as:

- `number`
- `bool`
- `word`
- `type`

### b) Functions

Functions are represented as `Function` statements and also by lambda terms when compiled.

Examples from the README include:

- `(n:number) => n + 1`
- `(b:bool, n:number, m:number) => if b then n else m`
- higher-order functions like `pred:number->bool`

The type system represents function signatures using exponent types like:

- `number -> number`
- `number -> bool`
- `number -> number -> number`

### c) Records and modules

The language supports record literals enclosed in `{ ... }` and also file inclusion with `include [...] for ...`.

This means the project is trying to support a module-like scoping concept. The compiler merges the imported record definitions into the active program, effectively allowing reusable constants and functions.

### d) Type system features

The type system is more advanced than a basic ML type checker. It supports:

- unions: `A | B`
- intersections: `A & B`
- function types: `A -> B`
- records/structs
- aliasing of types via `TypeDefinition`
- match-driven type extraction
- pattern-based union narrowing

This makes the project closer to a research/prototype typed language than a production compiler.

### e) Pattern matching and branching

The AST includes `Branch`, `Match`, and `Binary` operations. The type checker knows how to validate `if` expressions and some structured match logic.

The README also includes a match-expression example where a primitive type is split by runtime tags and the compiler produces a flattened union path.

---

## 6. How compilation works in practice

A typical usage flow looks like this:

1. Input is parsed in `Parser.fs`
2. AST is built as `Statement` and `Type` values
3. `Compiler.fs` turns the AST into either:
   - lambda calculus syntax tree
   - JavaScript expression strings
4. `Interpreter.fs` reduces lambda terms when necessary

The compiler tries to preserve semantics while translating. For example, booleans become Church booleans in Lambda Calculus, and function application is encoded as lambda application trees.

---

## 7. Important implementation notes

### `WIP` status

The project is clearly unfinished. The README explicitly says it is a work in progress and that “Lambda target is not yet up-to-date.”

That means:

- the parser is active
- the type system is ambitious
- the JavaScript backend is partially working
- the Lambda backend is evolving and may not match the newest source language exactly

### Custom design instead of standard tooling

This project does not rely on a mainstream parser generator or a standard compiler framework. Instead, it uses:

- custom parser combinators in `Parsec.fs`
- direct AST manipulation in F#
- ad hoc type-checking rules in `Checker.fs`

This makes the code highly educational and expressive, but also more brittle than a mature compiler toolchain.

### Multiple language layers

The project mixes:

- a user-facing DSL
- a type model
- an internal AST
- a lambda-calculus runtime representation
- generated JavaScript syntax

This layering is useful for experimentation, but it also means that the codebase is conceptually dense.

---

## 8. Examples of usage

The project is meant to be used in two main ways:

1. as an interactive terminal REPL
2. as a file-driven compiler that reads `.oxalc` source and emits lambda or JavaScript output

### 8.1 Interactive usage

The console app in `OxAlc.Console/Program.fs` creates a REPL loop. This is the simplest way to experiment with the language.

Command:

```bash
dotnet run --project OxAlc.Console/OxAlc.Console.fsproj -- --mode Terminal --back LCR
```

Example session:

```text
λ > let program :=
    let x : number = 2 in
    let y : number = 3 in
    x + y
end program
val it : number = 5
```

This works because:
- the parser reads the expression
- the type checker validates `number` types
- the compiler translates the expression into lambda form
- the lambda interpreter reduces it to the final value

---

### 8.2 File-based usage

You can also run one of the project’s sample files directly.

Command:

```bash
dotnet run --project OxAlc.Console/OxAlc.Console.fsproj -- --mode OxAlc.Console/Examples/programs1.oxalc --back JS
```

This reads the contents of `programs1.oxalc` and compiles it with the JavaScript backend.

The sample file is:

```fs
let program := 
    include [calculator] for
    let result := (calc . add)(1, 2) in 
    let custom_bin :binary_op= (a:number, b:number) => a + (b * 2) in
    custom_bin(result, result + 1)
end program
```

This demonstrates:
- `include` for loading modules
- record access via `calc . add`
- local binding with `let`
- higher-order function usage
- a custom function result returned as the program output

---

### 8.3 Using the sample modules

The repository includes sample files that model reusable modules and typed constants.

#### constants.oxalc

```fs
let constants := {
    zero  : number = 0;
    one   : number = 1;
    bool_t : bool = true;
    bool_f : bool = false;
} end constants
```

This is a number-and-boolean record module. It provides reusable constants for later programs.

#### functions.oxalc

```fs
let functions := {
    cond  := (b:bool, n:number, m:number) => if b then n else m;
    incr  := (n:number) => n + 1;
    decr  := (n:number) => n - 1;
    check := (pred:number->bool, n:number) => cond(pred(n), 1, 0);
    apply := (f:number->number, n:number) => f(n)
} end functions
```

This shows the language’s style for:
- function definitions
- function composition
- boolean-based branching
- higher-order values like `pred:number->bool`

#### operators.oxalc

```fs
let operators := 
    include [functions] for {
        ++  := (n:number) => incr(n);
        ??  := (b:bool, n:number, m:number) => cond(b, n, m);
        ==  := (n:number, m:number) => ((n = m) ?? 1)(0);
        <>  := (n:number, m:number) => ((n = m) ?? 0)(1);
        ?   := (n:number) => if n = 0 then false else true;
        |>  := (m:number, f:number->number) => f(m);
    }
end operators
```

This demonstrates the custom operator idea in the project: functions are used to model operators like `++`, `??`, `==`, and `|>`.

---

### 8.4 A more complete end-to-end example

This is the sort of program the README is aiming for:

```fs
let program := 
    include [constants, operators, calculator] for
    let value1 := num |> f where num := zero 
                       and f := incr in 
    let value2 := apply((n:number) => (calc . add)(n, 2), 0) in
    ?(value1 <> value2)
end program
```

Why this matters:
- it combines modules from multiple files
- it uses `include` to import dependencies
- it uses local `where ... and ...` bindings
- it composes functions in a way similar to functional programming
- it returns a boolean condition from the final expression

This is one of the clearest examples of the project’s intended style: a compact, algebraic, typed functional source language.

---

### 8.5 Running with the Lambda backend

If you want to run the lambda compiler path instead of JS output:

```bash
dotnet run --project OxAlc.Console/OxAlc.Console.fsproj -- --mode Terminal --back LCR
```

Then you can type a lambda-friendly expression such as:

```text
λ > let program :=
    let x : number = 2 in
    let y : number = 3 in
    x + y
end program
```

The backend then lowers the AST into lambda calculus and evaluates it through the interpreter.

This is the key idea behind the project: source syntax is function-first, but execution is rooted in lambda reduction.

---

### 8.6 A real project-scale JavaScript backend example (`JSR`)

This is the kind of program that looks like a real application rather than a toy transpile demo. It is a portfolio/risk engine built from several modules: configuration, pricing, risk analysis, and execution policy. It uses imported modules, record structures, type-based branching, and higher-order function composition, which is exactly where the JavaScript backend is most useful.

```fs
let config := {
    max_leverage : number = 4.0;
    min_margin   : number = 0.15;
    fee_rate     : number = 0.0025;
    cool_down    : number = 24;
    base_risk    : number = 0.12;
    stress_level : number = 1.5;
    zero         : number = 0;
    one          : number = 1;
    two          : number = 2;
    ten          : number = 10;
    hundred      : number = 100;
    truev        : bool = true;
    falsev       : bool = false;
} end config

let domain := {
    trade : type = {
        side     : word;
        symbol   : word;
        quantity : number;
        price    : number;
        leverage : number;
    };

    exposure : type = {
        gross   : number;
        net     : number;
        status  : word;
        limit   : number;
    };

    signal : type = number | bool | word;
} end domain

let mathlib := {
    add    := (a:number, b:number) => a + b;
    sub    := (a:number, b:number) => a - b;
    mul    := (a:number, b:number) => a * b;
    div    := (a:number, b:number) => a / b;
    abs    := (n:number) => if n < 0 then -n else n;
    max    := (a:number, b:number) => if a > b then a else b;
    min    := (a:number, b:number) => if a < b then a else b;
    clamp  := (low:number, high:number, x:number) =>
        if x < low then low else if x > high then high else x;
    choose := (flag:bool, a:number, b:number) => if flag then a else b;
    sqr    := (x:number) => x * x;
    scale  := (x:number, k:number) => x * k;
    isPos  := (x:number) => if x > 0 then true else false;
} end mathlib

let pricing :=
    include [config, mathlib] for {
        mark_to_market := (quantity:number, price:number) => mul(quantity, price);

        pnl := (entry:number, exit:number, qty:number) =>
            mul(sub(exit, entry), qty);

        fee := (notional:number) => mul(notional, fee_rate);

        spread_cost := (notional:number, spread:number) =>
            mul(notional, spread);

        value_score := (price:number, volatility:number) =>
            clamp(zero, hundred, mul(price, volatility));
    }
end pricing

let risk_engine :=
    include [config, mathlib, pricing] for {
        risk_factor := (leverage:number, volatility:number) =>
            add(base_risk, mul(leverage, volatility));

        check_margin := (exposure:number, equity:number) =>
            if div(exposure, equity) < min_margin then false else true;

        exposure_score := (qty:number, price:number, leverage:number) =>
            mul(mul(qty, price), leverage);

        risk_level := (qty:number, price:number, leverage:number, volatility:number) =>
            let exposure := exposure_score(qty, price, leverage) in
            let factor := risk_factor(leverage, volatility) in
            let score := mul(exposure, factor) in
            if score > 10000 then "high" else if score > 5000 then "medium" else "low";

        hedge_signal := (signal:number | bool | word) =>
            match signal with
            | (n:number) => if n > 0 then "buy" else "sell"
            | (b:bool) => if b then "long" else "short"
            | (w:word) => w;
    }
end risk_engine

let execution_policy :=
    include [config, mathlib, pricing, risk_engine] for {
        route_trade := (trade:trade, market_move:number) =>
            let gross := mark_to_market(trade.quantity, trade.price) in
            let spread := spread_cost(gross, 0.01) in
            let f := fee(gross) in
            let risk := risk_level(trade.quantity, trade.price, trade.leverage, market_move) in
            let should_trade := check_margin(add(gross, spread), mul(gross, 0.4)) in
            if should_trade then
                if risk = "high" then "hedge" else if risk = "medium" then "monitor" else "execute"
            else
                "pause";

        rebalance := (trade:trade, bias:number) =>
            let score := risk_level(trade.quantity, trade.price, trade.leverage, bias) in
            let adjusted := clamp(one, ten, add(trade.leverage, bias)) in
            if score = "high" then adjusted else trade.leverage;

        signal_to_action := (signal:number | bool | word) =>
            let action := hedge_signal(signal) in
            if action = "buy" then "open_long" else if action = "sell" then "open_short" else if action = "hedge" then "hedge_position" else "neutral";
    }
end execution_policy

let program :=
    include [config, domain, mathlib, pricing, risk_engine, execution_policy] for
    let entry := { side:"buy"; symbol:"AAPL"; quantity:150; price:210; leverage:2.5; } in
    let spread_cost_value := spread_cost(mark_to_market(entry.quantity, entry.price), 0.015) in
    let pnl_value := pnl(200, 218, 150) in
    let risk_value := risk_level(entry.quantity, entry.price, entry.leverage, 0.8) in
    let route := route_trade(entry, 0.8) in
    let action := signal_to_action(1) in
    if route = "execute" then add(pnl_value, spread_cost_value) else if action = "open_long" then pnl_value else 0
end program
```

Why this is a strong JSR example:
- it models a real application architecture, not a single expression
- it uses multiple imported modules to simulate a project pipeline
- it mixes record data, arithmetic, branch evaluation, and type-dispatched logic
- it exercises the JS backend in the way it is best suited: runtime object-like values and dispatch on dynamic data
- it is large enough to resemble the shape of an actual compiler-targeted application system

This is the version of JSR that feels like a “real big project” rather than a “language demo.”

---

### 8.7 A project-scale Lambda backend example (`LCRR`)

This is the kind of program that is realistic for the LCRR backend: a pure symbolic pipeline built from lambda abstractions, local bindings, arithmetic, and conditionals. It is not trying to emulate a GUI or full application runtime. Instead, it behaves like a large, functional computation pipeline whose entire logic can be reduced in a lambda evaluator.

```fs
let constants := {
    zero  : number = 0;
    one   : number = 1;
    two   : number = 2;
    three : number = 3;
    four  : number = 4;
    five  : number = 5;
    ten   : number = 10;
    twenty: number = 20;
    thirty: number = 30;
    fifty : number = 50;
    hundred : number = 100;
} end constants

let arithmetic :=
    include [constants] for {
        id     := (x:number) => x;
        add    := (a:number, b:number) => a + b;
        sub    := (a:number, b:number) => a - b;
        mul    := (a:number, b:number) => a * b;
        div    := (a:number, b:number) => a / b;
        sq     := (x:number) => mul(x, x);
        cube   := (x:number) => mul(x, sq(x));
        abs    := (x:number) => if x < 0 then -x else x;
        clamp  := (lo:number, hi:number, x:number) => if x < lo then lo else if x > hi then hi else x;
        max    := (a:number, b:number) => if a > b then a else b;
        min    := (a:number, b:number) => if a < b then a else b;
        step   := (x:number) => add(mul(x, two), three);
        ramp   := (x:number) => add(step(x), ten);
        score  := (x:number) => add(mul(x, five), twenty);
    }
end arithmetic

let logic :=
    include [constants] for {
        and_ := (a:bool, b:bool) => if a then b else false;
        or_  := (a:bool, b:bool) => if a then true else b;
        not_ := (a:bool) => if a then false else true;
        gt   := (a:number, b:number) => a > b;
        ge   := (a:number, b:number) => a >= b;
        lt   := (a:number, b:number) => a < b;
        eq   := (a:number, b:number) => a = b;
        is_positive := (x:number) => if x > 0 then true else false;
    }
end logic

let pipeline :=
    include [constants, arithmetic, logic] for {
        normalize := (x:number) =>
            let a := abs(x) in
            let b := clamp(zero, hundred, a) in
            if b > fifty then b - twenty else b;

        stabilize := (x:number) =>
            let s1 := sq(x) in
            let s2 := add(s1, three) in
            let s3 := mul(s2, two) in
            let s4 := clamp(zero, eighty, s3) in
            if s4 > forty then s4 - ten else s4;

        classify := (x:number) =>
            let n1 := normalize(x) in
            let n2 := stabilize(n1) in
            if n2 > sixty then "hot" else if n2 > thirty then "warm" else "cold";

        decision := (x:number) =>
            let n1 := normalize(x) in
            let n2 := stabilize(n1) in
            let safe := and_(is_positive(n2), ge(n2, ten)) in
            if safe then score(n2) else zero;
    }
end pipeline

let program :=
    include [constants, arithmetic, logic, pipeline] for
    let input := twenty in
    let a := step(input) in
    let b := ramp(a) in
    let c := normalize(b) in
    let d := stabilize(c) in
    let label := classify(d) in
    let value := decision(d) in
    if label = "warm" then value else add(value, ten)
end program
```

Why this is a strong LCRR example:
- it uses many nested local bindings, which is exactly how λ-evaluation thrives
- it stays entirely in the pure functional, arithmetic-first style of the lambda runtime
- it avoids JS-specific object/output assumptions
- it demonstrates layered computation: normalize → stabilize → classify → decide
- it is large enough to feel like a real computational pipeline, while still remaining valid for a lambda reduction engine

This is the correct shape for a serious LCRR project: a pure functional computation graph where each stage is a curried function and each value is reduced in the lambda runtime rather than emitted as JavaScript.

---

## 9. Summary

This project is a custom typed functional-language experiment built around the idea of compiling to lambda calculus and JavaScript. The main components are:

- `Definitions.fs`: AST and type model
- `Parser.fs`: source-language parser
- `Checker.fs`: type inference and validation
- `Compiler.fs`: backend code generation
- `Interpreter.fs`: lambda evaluator
- `Program.fs`: console REPL

In short, OxAlc is a compact prototype compiler platform for exploring a typed lambda-inspired language with syntax that resembles ML and functional JavaScript.

It is ambitious, educational, and clearly still under active development.

---

## 10. Best file order to read when learning the project

If you want to understand it quickly, read in this order:

1. `README.md`
2. `OxAlc.Service/Definitions.fs`
3. `OxAlc.Service/Parser.fs`
4. `OxAlc.Service/Checker.fs`
5. `OxAlc.Service/Compiler.fs`
6. `OxAlc.Service/Interpreter.fs`
7. `OxAlc.Console/Program.fs`

That path follows the actual life of a program from syntax to evaluation.
