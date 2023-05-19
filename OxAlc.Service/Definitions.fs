module Typedefinitions 

    
    type Expression = 
        | Term of string
        | Applicative of Expression * Expression
        | Lambda of Expression * Expression 
    and Envirement = list<string * Expression>
    and Thunk = Expression 
    
    type Type = 
        | Atom of string
        | Union of Type list
        | Intersection of Type list
        | Exponent of Type * Type
        | Struct of Map<Statement, Type>
        with override this.ToString() =
            let rec print_type type_expr = 
                match type_expr with
                | Atom name -> name
                | Union cons | Intersection cons-> List.foldBack (fun c s -> sprintf "%s | %s" s (c.ToString())) cons ""
                | Exponent(args, outs) -> sprintf "%s -> %s" (args.ToString()) (outs.ToString())
                | Struct _ -> failwith "not implemented yet"
            print_type this
    and TypingContext = Map<string, Type>
    
    and Statement = 
        (* Primitive Constructs*)
        | Value             of Literal 
        | Function          of (Statement * Type) list * Statement 
        | Application       of Statement * Statement list
        (* Derivative Constructs*)
        | Identifier        of string 
        | Bind              of Statement * Type * Statement * Statement 
        | TypeDefinition    of Statement * Type * Statement
        | Unary             of Operation * Statement
        | Binary            of Statement * Operation * Statement
        | Branch            of Statement * Statement * Statement
        | Compound          of Statement * ((Statement * Type) * Statement) list   
        | Context           of Statement list * Statement
    and Literal =
        | Bool of bool 
        | Variable of int 
        | String of List<char> 
        | List of List<Statement>  
        | Record of (Statement * Type * Statement) list
        | Tuple of List<Statement>
        | Constructor of (Statement * Statement)
    and Operation   =   Cons | Add | Subs | Div | Mult | Exp | Or | And | Eq | Neq | Lt | Not | Xor | Gt | YComb | Custom of string
                        static member toOp tokens =
                            match tokens with 
                            | ['*'] -> Mult  | ['/'] -> Div  | ['^'] -> Exp | ['+'] -> Add
                            | ['&'] -> And   | ['|'] -> Or   | ['~'] -> Not | ['!'] -> Xor 
                            | ['='] -> Eq    | ['<'] -> Lt   | ['>'] -> Gt  | ['-'] -> Subs
                            | ['Y'] -> YComb | ['@'] -> Cons | ['#'] -> Neq  
                            | _ -> Custom ( tokens |> List.map string |> String.concat "") 
    
    type Backend = 
        LCR | LLVM | MSIL | JS
        static member Parse str = 
            match str with 
            | "LCR" -> LCR
            | "LLVM" -> LLVM
            | "MSIL" -> MSIL
            | "JS" -> JS
            | _ -> failwith "Unknown backend"
    and  Result = 
        | LCRR  of Expression
        | JSR   of string
        | LLVMR of string
        | MSILR of string