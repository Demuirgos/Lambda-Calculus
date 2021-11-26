# OxaLC
	a basic ML like language with a Lambda-Calculus based Compiler
	
# LambdaCalculus Language Grammar :
	 Terminology := 
		| Application : "(λy.z x)" = (Lambda Terminology)
		| Lambda      : "λx.y"     = λName.Terminology 
		| Name        : "x"        = [a-z]+
		
# MLAbstraction Language Grammar :
	 Expression := 
		| Bind(Name, Expression)   : "let x := y in x" 
		| Func([Name], Expression) : "(y,w) => w"
		| Oper(Func, [Expression]) : "f(y,z)"
		| Branch(cnd, tCnt, fCnt)  : "if e then g else h"
		| Identifier 		   : [a-z]+
		| Literal 		   : true | false | [0..9]+
