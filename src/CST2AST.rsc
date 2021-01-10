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

AForm cst2ast(start[Form] sf) = cst2ast(sf.top);

AForm cst2ast(f:(Form)`form <Id name> { <Block* blocks> }`)
	= form("<name>", [cst2ast(b) | b <- blocks], src=f@\loc);

ABlock cst2ast(b:(Block)`<Question q>`)
	= block_basic(cst2ast(q), src=b@\loc);

ABlock cst2ast(b:(Block)`<Question q> = <Expr e>`)
	= block_init(cst2ast(q), cst2ast(e), src=b@\loc);
	
ABlock cst2ast(b:(Block)`{ <Block* bcks> }`)
	= block_list([cst2ast(bk) | bk <- bcks], src=b@\loc);
	
ABlock cst2ast(b:(Block)`if (<Expr e>) <Block bk>`)
	= block_if(cst2ast(e), cst2ast(bk), src=b@\loc);
	
ABlock cst2ast(b:(Block)`if (<Expr e>) <Block bk_t> else <Block bk_f>`)
	= block_ifelse(cst2ast(e), cst2ast(bk_t), cst2ast(bk_f), src=b@\loc);


AQuestion cst2ast(q:(Question)`<Str s> <Id i> : <Type t>`) 
 	= question(id("<s>", src=s@\loc), id("<i>", src=i@\loc), cst2ast(t), src=q@\loc);

AExpr cst2ast(e:(Expr)`<Id x>`) = ref(id("<x>", src=x@\loc), src=e@\loc);
AExpr cst2ast(e:(Expr)`<Str x>`) = \str("<x>", src=e@\loc);
AExpr cst2ast(e:(Expr)`<Int x>`) = \int(toInt("<x>"), src=e@\loc);
AExpr cst2ast(e:(Expr)`<Bool x>`) = \bool("<x>" == "true", src=e@\loc);
AExpr cst2ast(e:(Expr)`( <Expr x> )`) = expr_paranth(cst2ast(x), src=e@\loc);
AExpr cst2ast(e:(Expr)`! <Expr x>`) = expr_neg(cst2ast(x) src=e@\loc);
AExpr cst2ast(e:(Expr)`<Expr x> * <Expr y>`) = expr_mult(cst2ast(x), cst2ast(y), src=e@\loc);
AExpr cst2ast(e:(Expr)`<Expr x> / <Expr y>`) = expr_div(cst2ast(x), cst2ast(y), src=e@\loc);
AExpr cst2ast(e:(Expr)`<Expr x> + <Expr y>`) = expr_add(cst2ast(x), cst2ast(y), src=e@\loc);
AExpr cst2ast(e:(Expr)`<Expr x> - <Expr y>`) = expr_subtr(cst2ast(x), cst2ast(y), src=e@\loc);
AExpr cst2ast(e:(Expr)`<Expr x> \< <Expr y>`) = expr_less(cst2ast(x), cst2ast(y), src=e@\loc);
AExpr cst2ast(e:(Expr)`<Expr x> \<= <Expr y>`) = expr_leq(cst2ast(x), cst2ast(y), src=e@\loc);
AExpr cst2ast(e:(Expr)`<Expr x> \> <Expr y>`) = expr_greater(cst2ast(x), cst2ast(y), src=e@\loc);
AExpr cst2ast(e:(Expr)`<Expr x> \>= <Expr y>`) = expr_geq(cst2ast(x), cst2ast(y), src=e@\loc);
AExpr cst2ast(e:(Expr)`<Expr x> == <Expr y>`) = expr_eq(cst2ast(x), cst2ast(y), src=e@\loc);
AExpr cst2ast(e:(Expr)`<Expr x> != <Expr y>`) = expr_neq(cst2ast(x), cst2ast(y), src=e@\loc);
AExpr cst2ast(e:(Expr)`<Expr x> && <Expr y>`) = expr_and(cst2ast(x), cst2ast(y), src=e@\loc);
AExpr cst2ast(e:(Expr)`<Expr x> || <Expr y>`) = expr_or(cst2ast(x), cst2ast(y), src=e@\loc);

AType cst2ast(Type t) = \type("<t>", src=t@\loc);

