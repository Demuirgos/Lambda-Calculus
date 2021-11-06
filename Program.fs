open System
open Interpreter
open Abstractor

let REPL _ =
    Console.OutputEncoding <- Text.Encoding.Unicode
    let prefix = "λ >"
    let read = Console.ReadLine >> sprintf "%s"
    let eval = interpret
    let print= 
        function 
        | input ->  printfn "%s" input
                    prefix      
    let rec loop prefix = 
        prefix |> printf "%s"
        |> read |> eval |> print |> loop
    loop prefix

[<EntryPoint>]
let test _ =
    let rec loop () = 
        let input  = Console.ReadLine >> sprintf "%s"
        let result = Transpile << input <| ()
                    |> printf "%A\n"
        result |> loop
    loop ()
    0
