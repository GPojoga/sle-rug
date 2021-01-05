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

AForm cst2ast(f:(Form)`form <Id name> { <Block* blocks> }`){
	return form("<name>", [cst2ast(b) | b <- blocks], src=f@\loc);
}

ABlock cst2ast(Block b) {
	switch (b) {
		case (Block)`<Question q>`: 
			return block_basic(cst2ast(q), src=b@\loc);
		case (Block)`<Question q> = <Expr e>`: 
			return block_init(cst2ast(q), cst2ast(e), src=b@\loc);
		case (Block)`{ <Block* bcks> }`: 
			return block_list([cst2ast(bk) | bk <- bcks], src=b@\loc);
		case (Block)`if (<Expr e>) <Block bk>`:
			return block_if(cst2ast(e), cst2ast(bk), src=b@\loc);
		case (Block)`if (<Expr e>) <Block bk_t> else <Block bk_f>`:
			return block_ifelse(cst2ast(e), cst2ast(bk_t), cst2ast(bk_f), src=b@\loc);
		default: throw "Unhandled block <b>";
	}
}

AQuestion cst2ast(Question q) {
 	switch (q){
	  	case (Question)`<Str s> <Id i> : <Type t>`: 
	  		return question(id("<s>", src=s@\loc), id("<i>", src=i@\loc), cst2ast(t), src=q@\loc);
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

