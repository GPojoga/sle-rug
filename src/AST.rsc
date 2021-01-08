module AST

/*
 * Define Abstract Syntax for QL
 *
 * - complete the following data types
 * - make sure there is an almost one-to-one correspondence with the grammar
 */

data AForm(loc src = |tmp:///|)
  	= form(str name, list[ABlock] blocks)
  	; 

data AQuestion(loc src = |tmp:///|)
	= question(AId label, AId id, AType tp)
  	; 

data ABlock(loc src = |tmp:///|)
	= block_basic(AQuestion quest)
	| block_init(AQuestion quest, AExpr expr)
	| block_list(list[ABlock] blocks)
	| block_if(AExpr expr, ABlock block_t)
	| block_ifelse(AExpr expr, ABlock block_t, ABlock block_f)
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
