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
                | Union cons | Intersection cons -> 
                    let separator = match type_expr with Union _ -> " | " | _ -> " & "
                    System.String.Join(separator, List.map (fun i -> i.ToString()) cons)
                | Exponent(args, outs) -> sprintf "%s -> %s" (args.ToString()) (outs.ToString())
                | Struct kvp_map -> 
                    let fields_strings = 
                        Map.map(fun (Identifier(k)) v -> sprintf "%s: %s" k (v.ToString())) kvp_map
                        |> Map.values
                    sprintf "{ %s }" (System.String.Join(", ", fields_strings))
            print_type this
    and TypingContext = {
            Symbols: Map<string, Type>
            Types : Map<string, Type>
        }
        with static member PrimitivesMap = 
            Map.add "word" (Atom "type") 
            (Map.add "bool" (Atom "type") 
            (Map.add "number" (Atom "type") 
            (Map.add "type" (Atom "type") Map.empty))) 
            static member Empty = {
                Symbols = TypingContext.PrimitivesMap
                Types = Map.empty
            }
    
    and Statement = 
        (* Primitive Constructs*)
        | Value             of Literal 
        | Function          of (Statement * Type) list * Statement 
        | Application       of Statement * Statement list
        (* Derivative Constructs*)
        | Identifier        of string 
        | Bind              of Statement * Type * Statement * Statement 
        | Unary             of Operation * Statement
        | Binary            of Statement * Operation * Statement
        | Branch            of Statement * Statement * Statement
        | Match             of Statement * (Statement list)  * (Statement option)
        | Compound          of Statement * ((Statement * Type) * Statement) list 
        | Context           of Statement list * Statement
    and Literal =
        | Bool of bool 
        | Variable of int 
        | String of List<char> 
        | List of List<Statement>  
        | Record of (Statement * Type * Statement) list
        | Tuple of List<Statement>
        | TypeDefinition of Type 
    and Operation   =   Cons | Add | Subs | Div | Mult | Exp | Or | And | Eq | Neq | Lt | Not | Xor | Gt | YComb | Dot | Custom of string
                        static member toOp tokens =
                            match tokens with 
                            | ['*'] -> Mult  | ['/'] -> Div  | ['^'] -> Exp | ['+'] -> Add
                            | ['&'] -> And   | ['|'] -> Or   | ['~'] -> Not | ['!'] -> Xor 
                            | ['='] -> Eq    | ['<'] -> Lt   | ['>'] -> Gt  | ['-'] -> Subs
                            | ['Y'] -> YComb | ['@'] -> Cons | ['#'] -> Neq | ['.'] -> Dot
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