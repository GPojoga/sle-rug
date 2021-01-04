module Syntax

extend lang::std::Layout;
extend lang::std::Id;

/*
 * Concrete syntax of QL
 */

start syntax Form 
  	= "form" Id "{" Question* "}"
  	; 


syntax Question
  	= Str Id ":" Type ("=" Expr)?
  	| "{" Question* "}"
	| "if" "(" Expr ")" Question ("else" Question)?
  	; 

syntax Expr 
  	= Id \ "true" \ "false" // true/false are reserved keywords.
  	| Str
  	| Int
  	| Bool
  	| "(" Expr ")"
  	> "!" Expr
  	> left  ( Expr "*" Expr
	  		| Expr "/" Expr
	  		> Expr "+" Expr
	  		| Expr "-" Expr
	  		)
	> non-assoc ( Expr "\<" Expr
				| Expr "\<=" Expr
				| Expr "\>" Expr
				| Expr "\>=" Expr
				> Expr "==" Expr
				| Expr "!=" Expr 
				)
	> left  ( Expr "&&" Expr
			| Expr "||" Expr
			) 			 
	;
  
  
syntax Type
	= "integer"
	| "boolean"
	| "string" 
	;  
	
 
lexical Str 
	= "\"" ![\"]* "\"" 
	;


lexical Int 
  	= [1-9][0-9]*
  	| [0]
  	;


lexical Bool 
	= "true"
	| "false"
	;
	



