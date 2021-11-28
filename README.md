# OxaLC
	a basic ML like language with a Lambda-Calculus based Compiler
	
# LambdaCalculus Language Grammar :
	 Terminology := 
		| Application : "(λy.z x)" = (Lambda Terminology)
		| Lambda      : "λx.y"     = λName.Terminology 
		| Name        : "x"        = [a-z]+
		
# MLAbstraction Language Grammar :
	 Expression := 
		| Bind(x, y, e)		   : "let x := y in x" 
		| Func([y, w, ...], e)     : "(y,w) => e"
		| Oper(f, [y, z, ...])     : "f(y,z)"
		| BinOp(lhs, op, rhs)	   : lhs op rhs where op = '+' | '*' | '^' | '&' | '|'
		| Branch(e, g, h)	   : "if e then g else h"
		| Identifier 		   : [a-z]+
		| Literal 		   : true | false | [0..9]+
# OxaLC code Demo :
	let program := 
	    let n := 1 in
	    let f := (m, y) => y + m in
	    if true then    
		f(2, 3)
	    else    
		3
	end program 
