module Eval

import AST;
import Resolve;

/*
 * Implement big-step semantics for QL
 */
 
// NB: Eval may assume the form is type- and name-correct.


// Semantic domain for expressions (values)
data Value
  = vint(int n)
  | vbool(bool b)
  | vstr(str s)
  ;

// The value environment
alias VEnv = map[str name, Value \value];

// Modeling user input
data Input
  = input(str question, Value \value);
  
// produce an environment which for each question has a default value
// (e.g. 0 for int, "" for str etc.)
VEnv initialEnv(AForm f) {
	VEnv venv = (q.id.name : defVal(q.tp) | /AQuestion q <- f);
  	for (/ABlock _:block_init(AQuestion q, AExpr e) <- f) //update form with constants
  		venv = eval(f, input(q.id.name, eval(e, venv)), venv);
  	return venv;
}


// Because of out-of-order use and declaration of questions
// we use the solve primitive in Rascal to find the fixpoint of venv.
VEnv eval(AForm f, Input inp, VEnv venv) {
  return solve (venv) {
    venv = evalOnce(f, inp, venv);
  }
}

VEnv evalOnce(AForm f, Input inp, VEnv venv) {
  for(ABlock b <- f.blocks){
  	venv = eval(b, inp, venv);
  }
  return venv;
}

VEnv eval(ABlock b, Input inp, VEnv venv) {
 	switch (b) {
  		case block_basic(AQuestion q):
  			if(inp.question == q.id.name) 
  				venv[inp.question] = inp.\value;
  		case block_init(AQuestion q, AExpr e):
  			venv[q.id.name] = eval(e, venv);
  		case block_list(list[ABlock] blocks):
  			for (ABlock bk <- blocks)
  				venv = eval(bk, inp, venv);
  		case block_if(AExpr expr, ABlock bt):
  			if (eval(expr, venv).b) 
  				venv = eval(bt, inp, venv);
  		case block_ifelse(AExpr expr, ABlock bt, ABlock bf):
  			if (eval(expr, venv).b)
  				venv = eval(bt, inp, venv);
  			else
  				venv = eval(bf, inp, venv);
  		default: throw "Unknown block type <b>";
  	}
  return venv;
}

Value eval(AExpr e, VEnv venv) {
	switch (e) {
		case ref(id(str x)): return venv[x];
    	case \str(str val): return vstr(val);
    	case \int(int val): return vint(val);
    	case \bool(bool val): return vbool(val);
    	case expr_paranth(AExpr expr): return eval(expr, venv);
    	case expr_neg(AExpr expr): return vbool(!eval(expr, venv).b);
  	}
  
  	Value base = eval(e.left, venv);
	left = 	vint(_)  := base ? base.n:
			vbool(_) := base ? base.b:
							   base.s;
	right = vint(_)  := base ? eval(e.right, venv).n:
			vbool(_) := base ? eval(e.right, venv).b:
							   eval(e.right, venv).s;
							   
  	switch(e) {
    	case expr_mult(_,_): 	return vint(left * right); //it compiles correctly
   		case expr_div(_,_):  	return vint(left / right);
   		case expr_add(_,_):		return vint(left + right);
   		case expr_subtr(_,_): 	return vint(left - right);
   		case expr_less(_,_): 	return vbool(left < right);
   		case expr_leq(_,_): 	return vbool(left <= right);
   		case expr_greater(_,_): return vbool(left > right);
   		case expr_geq(_,_): 	return vbool(left >= right);
   		case expr_eq(_,_): 		return vbool(left == right); 
   		case expr_neq(_,_): 	return vbool(left != right); 
   		case expr_and(_,_): 	return vbool(left && right); 
   		case expr_or(_,_): 		return vbool(left || right); 
    	default: throw "Unsupported expression <e>";
  }
}

Value defVal(AType t) {
	switch (t) {
		case \type("integer") : return vint(0);
		case \type("boolean") : return vbool(false);
		case \type("string") : return vstr("");
		default: throw "Unknown type <t>";
	}
}
