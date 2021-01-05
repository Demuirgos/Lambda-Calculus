open System
open Parsec
open Interpreter

[<EntryPoint>]
let main (args:string []) =
    let word = "Lake" 
    Parser {
        let w = Seq.toList word
        let p1 = expect 'L'
        let p2 = expect 'a'
        return run w (p1 <|> p2)
    }
    |> printf "%A"
    0