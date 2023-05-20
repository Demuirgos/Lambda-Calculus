open System
open System.IO
open FSharp.Core
open Typedefinitions
open Interpreter
open OxalcParser
open OxalcCompiler

let parseArguments args = 
    let rec setDefaults defaults map= 
        match defaults with 
        | [] -> map
        | (k, v)::t -> 
            setDefaults t
                (if not(Map.containsKey k map) then 
                    Map.add k v map
                 else map)
                 

    let rec loop args map = 
        match args with 
        | [] -> map
        | h::(s::t) -> 
            loop t (Map.add h s map)
    loop args Map.empty
    |> setDefaults [("--mode", "Terminal"); ("--back", "LCR")]

type Mode = File of string | Terminal | Lambda
    with static member fromString str = 
        match str with 
        | "Lambda" -> Lambda
        | "Terminal" -> Terminal
        | _ -> File str

[<EntryPoint>]
let REPL args =
    let execute mode backend= 
        Console.OutputEncoding <- Text.Encoding.Unicode
        let prefix = "λ >"
        let read =  function
                    | File(path) -> fun () -> [yield! File.ReadLines(path)]
                                              |> String.concat "\n"
                    | _ -> fun () -> "let program := include [] for " + Console.ReadLine() + " end program" 
        let eval mode = 
            let operation = match mode with 
                            | Lambda -> Interpreter.parse
                            | _ ->  transpile backend
            operation >> function 
                | Ok (codeResult,_) -> 
                    match codeResult with 
                    | LCRR(code) -> code |> (interpret >> toString)
                    | JSR(code) -> code 
                    | _ -> "Invalid program"
                | error-> error |> toResult
        let print= 
            function 
            | input ->  printfn "%s" input
                        prefix      
        let rec loop state prefix =
            prefix |> printf "%s "
            |> read(state) |> eval(state)
            |> print |> loop (match state with 
                              | Lambda -> Lambda
                              | _ -> Terminal )
        loop mode prefix

    let parsedArgs = parseArguments (Array.toList args)

    let mode = Mode.fromString <| Map.find "--mode" parsedArgs
    let back = Backend.Parse <| Map.find "--back" parsedArgs

    execute mode back
