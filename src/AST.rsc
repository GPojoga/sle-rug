module AST

/*
 * Define Abstract Syntax for QL
 *
 * - complete the following data types
 * - make sure there is an almost one-to-one correspondence with the grammar
 */

data AForm(loc src = |tmp:///|)
  	= form(str name, list[AQuestion] questions)
  	; 

data AQuestion(loc src = |tmp:///|)
	= question(str name, AId id, AType tp)
	| question(str name, AId id, AType tp, AExpr expr)
	| questions(list[AQuestion] blocks)
	| \if(AExpr expr, AQuestion quest_t)
	| if_else(AExpr expr, AQuestion quest_t, AQuestion quest_f)
  	; 
	
data AExpr(loc src = |tmp:///|)
  	= ref(AId id)
	| \str(str str_val)
	| \int(int int_val)
	| \bool(bool bool_val)
	| expr_paranth(AExpr expr)
	| expr_neg(AExpr expr)
	| expr_mult(AExpr left, AExpr right)
	| expr_div(AExpr left, AExpr right)
	| expr_add(AExpr left, AExpr right)
	| expr_subtr(AExpr left, AExpr right)
	| expr_less(AExpr left, AExpr right)
	| expr_leq(AExpr left, AExpr right)
	| expr_greater(AExpr left, AExpr right)
	| expr_geq(AExpr left, AExpr right)
	| expr_eq(AExpr left, AExpr right)
	| expr_neq(AExpr left, AExpr right)
	| expr_and(AExpr left, AExpr right)
	| expr_or(AExpr left, AExpr right)
	;

data AId(loc src = |tmp:///|)
  	= id(str name);

data AType(loc src = |tmp:///|)
	= \type(str val);
