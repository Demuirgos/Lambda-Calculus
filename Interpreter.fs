module Interpreter
    type Token =
        | Parenthese of char
        | Dot 
        | Identifier
        | Dispatcher
    type Expression = 
        | Atom of string
        | Applicative of Expression * Expression
        | Lambda of string * Expression 
    and Envirement = string * Expression

    let Parse (input:char list) = 
        let tokenize rawExpression = failwith "not yet made"
        failwith "not yet made"
