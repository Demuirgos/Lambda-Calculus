# Lambda-Calculus (WIP)
A simple Programming Language Compiler to Lambda-Calculus, with a Lambda-Runtime 
Oxalc Example :

 * in a separate file called functions.oxalc :
```fs
let functions := {
        null  := (b:bool, n:number) => if b then n else 0;  
        incr  := (n:number) => n + 1;
        decr  := (n:number) => n - 1;
        check := (pred:number->bool, n:number) => null(pred(n), 1);
        apply := (f:number->number, n:number) => f(n)
    }
end functions
```
* in a separate file called constants.oxalc :
```fs
let constants := {
        zero  : number = 0;
        one   : number = 1;
        bool_t : bool = true;
        bool_f : bool = false 
    }
end constants 
```
* in a separate file called operators.oxalc :
```fs
let operators := 
    include [functions] for {
        ++    := (n:number) => incr(n);
        ??    := (b:bool, n:number) => null(b, n);
        ==    := (n:number, m:number) => (n = m) ?? 1;
        <>    := (n:number, m:number) => (n = m) ?? 0;
        |>    := (m:number, f:number->number) => f(m)
    }
end operators
```
* in our main file :
```fs
let program := 
    include [constants, operators] for
    let value1 := num |> f where num := zero 
                           and f := incr in 
    let value2 := apply((n:number) => n + 2, 0) in
    value1 <> value2
end program 
```
this program yields the following result : 
```fs
val it : bool = λ_a.λ_b._a
```
