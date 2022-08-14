# Lambda-Calculus (WIP)
A simple Programming Language Compiler to Lambda-Calculus, with a Lambda-Runtime 
Oxalc Example :

 * in a separate file called functions.oxalc :
```fsharp
let functions := {
        null  := (b:bool, n:number) => if b then n else 0;  
        incr  := (n:number) => n + 1;
        decr  := (n:number) => n - 1;
        check := (pred:number->bool, n:number) => null(pred(n), 1)
    }
end functions
```
* in a separate file called constants.oxalc :
```fsharp
let constants := {
        zero  : number = 0;
        one   : number = 1;
        bool_t : bool = true;
        bool_f : bool = false 
    }
end constants 
```
* in a separate file called operators.oxalc :
```fsharp
let operators := 
    include [functions] for {
        ++    := (n:number) => incr(n);
        ??    := (b:bool, n:number) => null(b, n);
        ==    := (n:number, m:number) => (n = m) ?? 1
    }
end operators
```
* in our main file :
```fsharp
let program := 
    include [constants, operators] for
    let value := f(num) where num := zero 
                        and f := incr in 
    ++(value) == 2
end program 
```
this program yields the following result : 
```
val it : bool = λ_a.λ_b._a
```
