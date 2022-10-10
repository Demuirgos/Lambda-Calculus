module Generator2
    open System.Collections.Generic
    open System.IO
    open Typedefinitions
    open Typechecker
    open OxalcParser
    open System.Text.RegularExpressions

    type EthereumInstructions = 
        | STOP = 0x00
        | ADD = 0x01
        | MUL = 0x02
        | SUB = 0x03
        | DIV = 0x04
        | SDIV = 0x05
        | MOD = 0x06
        | SMOD = 0x07
        | ADDMOD = 0x08
        | MULMOD = 0x09
        | EXP = 0x0a
        | SIGNEXTEND = 0x0b

        | LT = 0x10
        | GT = 0x11
        | SLT = 0x12
        | SGT = 0x13
        | EQ = 0x14
        | ISZERO = 0x15
        | AND = 0x16
        | OR = 0x17
        | XOR = 0x18
        | NOT = 0x19
        | BYTE = 0x1a
        | SHL = 0x1b // EIP-145
        | SHR = 0x1c // EIP-145
        | SAR = 0x1d // EIP-145

        | SHA3 = 0x20

        | ADDRESS = 0x30
        | BALANCE = 0x31
        | ORIGIN = 0x32
        | CALLER = 0x33
        | CALLVALUE = 0x34
        | CALLDATALOAD = 0x35
        | CALLDATASIZE = 0x36
        | CALLDATACOPY = 0x37
        | CODESIZE = 0x38
        | CODECOPY = 0x39
        | GASPRICE = 0x3a
        | EXTCODESIZE = 0x3b
        | EXTCODECOPY = 0x3c
        | RETURNDATASIZE = 0x3d
        | RETURNDATACOPY = 0x3e
        | EXTCODEHASH = 0x3f

        | BLOCKHASH = 0x40
        | COINBASE = 0x41
        | TIMESTAMP = 0x42
        | NUMBER = 0x43
        | PREVRANDAO = 0x44
        | GASLIMIT = 0x45
        | CHAINID = 0x46
        | SELFBALANCE = 0x47
        | BASEFEE = 0x48

        | POP = 0x50
        | MLOAD = 0x51
        | MSTORE = 0x52
        | MSTORE8 = 0x53
        | SLOAD = 0x54
        | SSTORE = 0x55
        | JUMP = 0x56
        | JUMPI = 0x57
        | PC = 0x58
        | MSIZE = 0x59
        | GAS = 0x5a
        | JUMPDEST = 0x5b
        | BEGINSUB = 0x5c
        | RETURNSUB = 0x5d
        | JUMPSUB = 0x5e

        | PUSH1 = 0x60
        | PUSH2 = 0x61
        | PUSH3 = 0x62
        | PUSH4 = 0x63
        | PUSH5 = 0x64
        | PUSH6 = 0x65
        | PUSH7 = 0x66
        | PUSH8 = 0x67
        | PUSH9 = 0x68
        | PUSH10 = 0x69
        | PUSH11 = 0x6a
        | PUSH12 = 0x6b
        | PUSH13 = 0x6c
        | PUSH14 = 0x6d
        | PUSH15 = 0x6e
        | PUSH16 = 0x6f
        | PUSH17 = 0x70
        | PUSH18 = 0x71
        | PUSH19 = 0x72
        | PUSH20 = 0x73
        | PUSH21 = 0x74
        | PUSH22 = 0x75
        | PUSH23 = 0x76
        | PUSH24 = 0x77
        | PUSH25 = 0x78
        | PUSH26 = 0x79
        | PUSH27 = 0x7a
        | PUSH28 = 0x7b
        | PUSH29 = 0x7c
        | PUSH30 = 0x7d
        | PUSH31 = 0x7e
        | PUSH32 = 0x7f

        | DUP1 = 0x80
        | DUP2 = 0x81
        | DUP3 = 0x82
        | DUP4 = 0x83
        | DUP5 = 0x84
        | DUP6 = 0x85
        | DUP7 = 0x86
        | DUP8 = 0x87
        | DUP9 = 0x88
        | DUP10 = 0x89
        | DUP11 = 0x8a
        | DUP12 = 0x8b
        | DUP13 = 0x8c
        | DUP14 = 0x8d
        | DUP15 = 0x8e
        | DUP16 = 0x8f

        | SWAP1 = 0x90
        | SWAP2 = 0x91
        | SWAP3 = 0x92
        | SWAP4 = 0x93
        | SWAP5 = 0x94
        | SWAP6 = 0x95
        | SWAP7 = 0x96
        | SWAP8 = 0x97
        | SWAP9 = 0x98
        | SWAP10 = 0x99
        | SWAP11 = 0x9a
        | SWAP12 = 0x9b
        | SWAP13 = 0x9c
        | SWAP14 = 0x9d
        | SWAP15 = 0x9e
        | SWAP16 = 0x9f

        | LOG0 = 0xa0
        | LOG1 = 0xa1
        | LOG2 = 0xa2
        | LOG3 = 0xa3
        | LOG4 = 0xa4

        | TLOAD = 0xb3
        | TSTORE = 0xb4

        | CREATE = 0xf0
        | CALL = 0xf1
        | CALLCODE = 0xf2
        | RETURN = 0xf3
        | DELEGATECALL = 0xf4
        | CREATE2 = 0xf5
        | STATICCALL = 0xfa
        | REVERT = 0xfd
        | INVALID = 0xfe
        | SELFDESTRUCT = 0xff

    let rec emitCode rawInput = 
        let SymbolTable = Dictionary<_,_>()
        let input = 
            match rawInput with 
            | Comment(comments) ->
                let rec removeComments (str:string) (comments: Capture list) =  
                    match comments with
                    | [] -> str
                    | h::t -> removeComments (str.Remove(h.Index, h.Length)) t
                removeComments rawInput comments
            | _ -> rawInput
        let rec curry =
            function 
            | Function ([_] , _ ) as input -> input
            | Function ((h::t),body) -> Function ([h], curry <| Function(t, body))
            | _ -> failwith "Expression cannot be curried"
        let parseExp arg = (fromStr arg, parseExpr) ||> run 
        let Result = parseExp input
        match Result with 
        | Ok (program,r)-> 
            let mutable bytecode = System.Collections.Generic.List<byte>()
            let rec emitLambda = function
                | Context(files, program) ->
                    let program = 
                        match files with
                        | [] -> Ok program
                        | _  ->  
                            let filesContents = List.map ( function Identifier(path) -> path, path |> ((sprintf "%s.oxalc") >> File.ReadAllText >> parseExp))
                            let all = 
                                let folder = fun state (file, next) -> 
                                    match (state, next) with 
                                    | (Ok ys, Ok (n, _)) -> ys @ [n] |> Ok
                                    | Error errAcc, Error errNew  -> Error (errNew::errAcc)
                                    | Error _ as error, _ -> error 
                                    | _, Error err  -> Error [err]
                                Seq.fold folder (Ok []) 

                            let rec injectFields fields p= 
                                match fields with 
                                | (fd_id, fd_t, fd_body)::t -> 
                                    Bind(fd_id, fd_t, fd_body, injectFields t p)
                                | _ -> p

                            let rec wrapFiles filesAst program = 
                                match filesAst with 
                                | []   -> Ok program
                                | Bind(_, _ , Context(dependencies, Library(fields)), _)::t ->
                                    let dependenciesContent = all (filesContents dependencies)
                                    match dependenciesContent with
                                    | Ok ds -> 
                                        match wrapFiles ds (injectFields fields program) with
                                        | Ok result -> wrapFiles t result
                                        | Error err -> Error err
                                    | Error msg -> Error msg
                                | Bind(_, _ , Library(fields), _)::t ->
                                    wrapFiles t (injectFields fields program)
                                | _ -> Error [("Import Error","Invalid file structure", None)]
                            match all (filesContents files) with
                            | Ok (Asts)-> wrapFiles (Asts) program 
                            | Error err -> Error err
                    match program with
                    | Ok program -> 
                        let typeResult = TypeOf Map.empty program 
                        match typeResult with
                        | Ok(expr_type) -> 
                            printf "val it : %s = " (expr_type.ToString())
                            emitLambda program
                        | Error msg -> failwith msg
                    | Error msgs -> 
                        let rec msgAcc msgs = 
                            match msgs with
                            | [(label, msg, _)] -> sprintf "%s: %s" label msg
                            | (label, msg, _)::errs -> 
                                sprintf "%s: %s \n%s" label msg (msgAcc errs)
                        failwith (msgAcc msgs)
                | Bind(Identifier(name),_,  expr, value) ->
                    let address = SymbolTable.Count * 32
                    SymbolTable.Add(name, address)
                    bytecode.AddRange(EthereumInstructions.PUSH1, address)
                    bytecode.AddRange(EthereumInstructions.PUSH1, 0)
                    bytecode.Add(EthereumInstructions.MSTORE)

                    

                | Compound(expr, binds) as e -> 

                | Function _ as f->

                | Application(expr, args) as a ->

                | Identifier(name) -> Term name
                | Unary(Op, rhs) -> 

                | Branch(cond,tClause, fClause) as (t: Statement) -> 

                | Binary(lhs, op, rhs) ->
                    match op  with 
                    | Add -> 
                    
                    | Mult-> 
                    
                    | Exp -> 
                    
                    | And -> 
                    
                    | Or  -> 
                    
                    | Subs-> 
                    
                    | Lt  -> 
                    
                    | Eq  -> 
                    
                    | Xor -> 
                    
                    | Custom(token) -> 
                    
                    | Cons -> 
                    
                    | Div  -> 
                    
                    | Not  -> 
                    
                    | YComb -> failwith "Not Implemented"
                | Value(var) -> 
                    match var with 
                    | Bool booleanExpr -> 

                    | Variable(var) ->

                    | _ as v -> failwithf "%A is not supported by Lambda-Calculus" v 
                | _  -> failwith "Not Implemented or Not Compilable"
            Ok (EVM(emitLambda program), ``initial State``)