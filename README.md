# Lambda-Calculus (WIP)
A simple Programming Language Compiler to Lambda-Calculus, with a Lambda-Runtime 
Oxalc Example :

 * in a separate file called functions.oxalc :
```fs
let functions := {
        cond  := (b:bool, n:number, m:number) => if b then n else m;  
        incr  := (n:number) => n + 1;
        decr  := (n:number) => n - 1;
        check := (pred:number->bool, n:number) => cond(pred(n), 1, 0);
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
        ??    := (b:bool, n:number, m:number) => cond(b, n, m);
        ==    := (n:number, m:number) => ((n = m) ?? 1)(0);
        <>    := (n:number, m:number) => ((n = m) ?? 0)(1);
        ?     := (n:number) => if n = 0 then false else true;
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
    ?(value1 <> value2)
end program 
```
this program yields the following result : 
```fs
val it : bool = λ_a.λ_b._a
```
```js
var it = ((program) => program)(((cond) => ((incr) => ((decr) => ((check) => ((apply) => ((addadd) => ((qstqst) => ((eqleql) => ((grtlet) => ((qst) => ((ordlet) => ((zero) => ((one) => ((bool_t) => ((bool_f) => ((value1) => ((value2) => qst(grtlet(value1, value2)))(apply((n) => n + 2)(0)))(((num) => ((f) => ordlet(num, f))(incr))(zero)))(false))(true))(1))(0))((m) => (f) => f(m)))((n) => ((thenb, elseb) => (n == 0) ? thenb() : elseb())(() => false, () => true)))((n) => (m) => qstqst(n == m, 0)(1)))((n) => (m) => qstqst(n == m, 1)(0)))((b) => (n) => (m) => cond(b)(n)(m)))((n) => incr(n)))((f) => (n) => f(n)))((pred) => (n) => cond(pred(n))(1)(0)))((n) => n - 1))((n) => n + 1))((b) => (n) => (m) => ((thenb, elseb) => (b) ? thenb() : elseb())(() => n, () => m))) // true
```
