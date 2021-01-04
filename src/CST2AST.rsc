module CST2AST

import Syntax;
import AST;

import ParseTree;
import String;

/*
 * Implement a mapping from concrete syntax trees (CSTs) to abstract syntax trees (ASTs)
 *
 * - Use switch to do case distinction with concrete patterns (like in Hack your JS) 
 * - Map regular CST arguments (e.g., *, +, ?) to lists 
 *   (NB: you can iterate over * / + arguments using `<-` in comprehensions or for-loops).
 * - Map lexical nodes to Rascal primitive types (bool, int, str)
 * - See the ref example on how to obtain and propagate source locations.
 */

AForm cst2ast(start[Form] sf) {
  return cst2ast(sf.top); 
}

AForm cst2ast(f:(Form)`form <Id name> { <Question* questions> }`){
	return form("<name>", [cst2ast(q) | q <- questions], src=f@\loc);
}

AQuestion cst2ast(Question q) {
 	switch (q){
  	case (Question)`<Str s> <Id i> : <Type t>`: 
  		return question("<s>", id("<i>", src=i@\loc), cst2ast(t), src=q@\loc);
  	case (Question)`<Str s> <Id i> : <Type t> = <Expr e>`: 
  		return question("<s>", id("<i>", src=i@\loc), cst2ast(t), cst2ast(e), src=q@\loc);
  	case (Question)`{ <Question* qst> }`:
  		return questions([cst2ast(qn) | qn <- qst], src=q@\loc);
  	case (Question)`if ( <Expr e> ) <Question qst> `: 
		return \if(cst2ast(e), cst2ast(qst), src=q@\loc);
	case (Question)`if ( <Expr e> ) <Question qst1> else <Question qst2>`: 
		return if_else(cst2ast(e), cst2ast(qst1), cst2ast(qst2), src=q@\loc);
  default: throw "Unhandled question: <q>";
  }
}

AExpr cst2ast(Expr e) {
  switch (e) {
    case (Expr)`<Id x>`: 
    	return ref(id("<x>", src=x@\loc), src=x@\loc);
    case (Expr)`<Str x>`: 
    	return \str("<x>", src=x@\loc);
    case (Expr)`<Int x>`: 
    	return \int(toInt("<x>"), src=x@\loc);
    case (Expr)`<Bool x>`: 
    	return \bool("<x>" == "true", src=x@\loc);
    case (Expr)`( <Expr x> )`: 
    	return expr_paranth(cst2ast(x), src=e@\loc);
	case (Expr)`! <Expr x>` : 
		return expr_neg(cst2ast(x) src=e@\loc);
	case (Expr)`<Expr x> * <Expr y>` : 
		return expr_mult(cst2ast(x), cst2ast(y), src=e@\loc);
	case (Expr)`<Expr x> / <Expr y>` : 
		return expr_div(cst2ast(x), cst2ast(y), src=e@\loc);
	case (Expr)`<Expr x> + <Expr y>` : 
		return expr_add(cst2ast(x), cst2ast(y), src=e@\loc);
	case (Expr)`<Expr x> - <Expr y>` : 
		return expr_subtr(cst2ast(x), cst2ast(y), src=e@\loc);
	case (Expr)`<Expr x> \< <Expr y>` : 
		return expr_less(cst2ast(x), cst2ast(y), src=e@\loc);
	case (Expr)`<Expr x> \<= <Expr y>` : 
		return expr_leq(cst2ast(x), cst2ast(y), src=e@\loc);
	case (Expr)`<Expr x> \> <Expr y>` : 
		return expr_greater(cst2ast(x), cst2ast(y), src=e@\loc);
	case (Expr)`<Expr x> \>= <Expr y>` : 
		return expr_geq(cst2ast(x), cst2ast(y), src=e@\loc);
	case (Expr)`<Expr x> == <Expr y>` : 
		return expr_eq(cst2ast(x), cst2ast(y), src=e@\loc);
	case (Expr)`<Expr x> != <Expr y>` : 
		return expr_neq(cst2ast(x), cst2ast(y), src=e@\loc);
	case (Expr)`<Expr x> && <Expr y>` : 
		return expr_and(cst2ast(x), cst2ast(y), src=e@\loc);
	case (Expr)`<Expr x> || <Expr y>` : 
		return expr_or(cst2ast(x), cst2ast(y), src=e@\loc);
    default: throw "Unhandled expression: <e>";
  }
}

AType cst2ast(Type t) {
	return \type("<t>", src=t@\loc);
}

