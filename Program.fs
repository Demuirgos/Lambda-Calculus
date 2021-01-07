open System
open Parsec
open Interpreter

[<EntryPoint>]
let main (args:string []) =
    let word = "-35.3" 
    //let verbParser = Parser {
    //let subject = Parser {
    //        let proc = fun w -> w |> (Seq.toList >> allOf)
    //        return! (proc "the") .>>. expect ' ' .>>. (proc "lake") .>>. expect ' '
    //    }
    //    let verb = Parser {
    //        let p = expect 'i' .>>. expect 's'
    //        return p
    //    } 
    //    let object = (expect ' ') .>>. (allOf <| Seq.toList "dry" )
    //    return between subject verb object
    //}
    //((Seq.toList word),verbParser) ||> run  |> printf "%A"
    let pint = 
        let intp = "0123456789" |> Seq.toList |> anyOf |> many 1 
        let signp = option (expect '-')
        let sepp = expect '.'
        let pnumber = intp .>>. option (sepp .>>. intp)
        let mapping (((sign,part1),point),part2) = 
            let arr2Str chars = chars |> (List.map string) |> List.reduce (+) 
            let absValue = sprintf "%s%c%s" (arr2Str part1) (point) (arr2Str part2) |> float
            match sign with 
            | Some _ -> - absValue
            | _ -> absValue
        signp .>>. intp .>>. sepp .>>. intp 
        |> map mapping
    (Seq.toList word,pint) ||> run  |> printf "%A"
    0