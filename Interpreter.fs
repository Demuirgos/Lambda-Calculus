module Interpreter
    type Expression = 
        | Atom of string
        | Applicative of Expression * Expression
        | Lambda of string * Expression 
    and Envirement = string * Expression

    let Parse (input:char list) = 
        0