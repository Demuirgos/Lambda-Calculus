open System
open Parsec
open Interpreter

[<EntryPoint>]
let main (args:string []) =
    let word = "the lake is dry" 
    let verbParser = Parser {
        let subject = Parser {
            let p w = allOf (Seq.toList w) 
            return! (p "the") .>>. expect ' ' .>>. (p "lake") .>>. expect ' '
        }
        let verb = Parser {
            let p = expect 'i' .>>. expect 's'
            return! p
        } 
        let object = (expect ' ') .>>. (allOf <| Seq.toList "dry" )
        return! between subject verb object
    }
    ((Seq.toList word),verbParser) ||> run  |> printf "%A"
    0