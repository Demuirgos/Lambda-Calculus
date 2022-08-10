module Typedefinitions 

    
    type Expression = 
        | Term of string
        | Applicative of Expression * Expression
        | Lambda of Expression * Expression 
    and Envirement = list<string * Expression>
    and Thunk = Expression 
    
    type Type = 
        | Atom of string
        | Arrow of param : Type * body  : Type
    and Type_t = 
        | Const of Type
        | Union of Type_t * Type_t
        | Intersection of Type_t * Type_t
        | Exponent of Type_t * Type_t
    and TypingContext = Map<string, Type>
    
    type Statement = 
        (* Primitive Constructs*)
        | Value             of Literal 
        | Function          of (Statement * Type) list * Statement 
        | Application       of Statement * Statement list
        (* Derivative Constructs*)
        | Identifier        of string 
        | Bind              of Statement * Type * Statement * Statement 
        | Typedefinition    of Statement * Type_t
        | Unary             of Operation * Statement
        | Binary            of Statement * Operation * Statement
        | Branch            of Statement * Statement * Statement
        | Compound          of Statement * (Statement * Statement) list   
        | Context           of Statement list * Statement
    and Literal =
        | Hole | True | False 
        | Variable of int 
        | String of List<char> 
        | List of List<Statement>  
        | Record of Map<string,Statement>
    and Operation   =   Cons |Add | Subs | Div | Mult | Exp | Or | And | Eq | Lt | Not | Xor | Gt | YComb | Custom of string
                        static member toOp tokens =
                            match tokens with 
                            | ['*'] -> Mult  | ['/'] -> Div  | ['^'] -> Exp | ['+'] -> Add
                            | ['&'] -> And   | ['|'] -> Or   | ['~'] -> Not | ['!'] -> Xor 
                            | ['='] -> Eq    | ['<'] -> Lt   | ['>'] -> Gt  | ['-'] -> Subs
                            | [ 'Y'] -> YComb | ['@'] -> Cons | _ -> Custom ( tokens |> List.map string |> String.concat "") 
    type Backend = LCR | LLVM | MSIL
