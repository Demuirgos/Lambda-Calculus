# Lambda-Calculus (WIP)
A simple Programming Language Compiler to Lambda-Calculus, with a Lambda-Runtime 
Oxalc Example :
 
* in a separate file called operators.oxalc :
```fsharp
let operators := 
    include [functions] for { 
        ++    := (n : number) => incr(n);
        +=    := (n : number, m: number) => (n + 1) = m;
        ??    := (b : bool, n : number) => null(b, n);
    }
end operators
```
* in a separate file called functions.oxalc :
```fsharp
let functions := { 
        incr  := (n : number) => n + 1;
        decr  := (n : number) => n - 1;
        null  := (b : bool, n : number) => if b then n else 0;
        check := (pred:number->bool, n:number) => null(pred(n), 1)  
    }
end functions
```
* in a separate file called constants.oxalc :
```fsharp
let constants := { 
        zero  : number = 0;
        one   : number = 1;
        boolF : bool = false; 
        boolT : bool = true 
    }
end constants  
```
* in our main file :
```fsharp
let program := 
    include [constants, operators] for 
    let value := f(num) where num := zero 
                        and f := incr in 
    value += 2
end program 
```
this program yields the following result : 
```
val it : number = λ_a.λ_b._b
```
