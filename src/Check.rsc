module Check

import AST;
import Resolve;
import Message;
import Set;
import List;

data Type
  = tint()
  | tbool()
  | tstr()
  | tunknown()
  ;

// the type environment consisting of defined questions in the form 
alias TEnv = rel[loc def, str name, str label, Type \type];

TEnv collect(AForm f) {
  return {<q.id.src, q.id.name, q.label.name, getType(q.tp)> | /AQuestion q <- f};	 
}

set[Message] check(AForm f, TEnv tenv, UseDef useDef) {
  return { m | /ABlock 	  b <- f, m <- check(b, tenv, useDef)} +
  		 { m | /AExpr	  e <- f, m <- check(e, tenv, useDef)} +
  		 { m | /AQuestion q <- f, m <- check(q, tenv, useDef)};
}

set[Message] check(AQuestion q, TEnv tenv, UseDef _) {
  return { error("Duplicate question", q.id.src) | 
  								size({s | s <- tenv, s.name == q.id.name}) > 1 } +
  		 { warning("Duplicate label", q.label.src) | 
  		 						size({s | s <- tenv, s.label == q.label.name}) > 1 };
}

set[Message] check(ABlock b, TEnv tenv, UseDef useDef) {
	list[Type] expected = [];
	switch (b) {
		case block_init(AQuestion q, _): expected = [getType(q.tp)];
  		case block_if(_,_):			   	 expected = [tbool()];
  		case block_ifelse(_,_,_):	   	 expected = [tbool()];
	}
	if (isEmpty(expected)) return {};
	return expect(expr_paranth(b.expr, src=b.expr.src), tenv, useDef, expected);
}

set[Message] check(AExpr e, TEnv tenv, UseDef useDef) {
  list[Type] expected = [];
  
  switch (e) {
    case ref(AId x):
      	return { error("Undeclared question", x.src) | useDef[x.src] == {} };
	case expr_neg(_): 		expected = [tbool()];
	case expr_mult(_,_): 	expected = [tint()];
    case expr_div(_,_): 	expected = [tint()];
    case expr_add(_,_): 	expected = [tint()];
    case expr_subtr(_,_): 	expected = [tint()];
    case expr_less(_,_): 	expected = [tint(), tstr()];
	case expr_leq(_,_): 	expected = [tint(), tstr()];
	case expr_greater(_,_): expected = [tint(), tstr()];
	case expr_geq(_,_): 	expected = [tint(), tstr()];
	case expr_eq(_,_): 		expected = [tint(), tstr(), tbool()];
	case expr_neq(_,_): 	expected = [tint(), tstr(), tbool()];
	case expr_and(_,_): 	expected = [tbool()];
	case expr_or(_,_): 		expected = [tbool()];
  }
  if (isEmpty(expected)) return {};
  return expect(e, tenv, useDef, expected);
}

// determine if the subexpressions are of the expected type(s)
set[Message] expect(AExpr e, TEnv tenv, UseDef useDef, list[Type] expected) {
	operands = [expr | AExpr expr <- e];
	
	if (isEmpty(operands)) return {};
	
	Type baseType = typeOf(operands[0], tenv, useDef);
	
	for (l <- operands)
		if (typeOf(l, tenv, useDef) != baseType) 
			return { error("Operands must have the same type", e.src) }; 
	
	if (baseType in expected) return {};
	
	return {error("Given type <toStr(baseType)>. " +
		   		   "Allowed type(s) : " +
		   		   "<for (l <- expected) {><toStr(l)> <}>", e.src)};
}

Type typeOf(AExpr e, TEnv tenv, UseDef useDef) {
  switch (e) {
    case ref(id(_, src = loc u)):  
      if (<u, loc d> <- useDef, <d, _, _, Type t> <- tenv)
        return t;
    case \str(_): return tstr();
   	case \int(_): return tint();
   	case \bool(_): return tbool();
   	case expr_paranth(AExpr expr): return typeOf(expr, tenv, useDef);
   	case expr_neg(_): return tbool();
   	case expr_mult(_,_): return tint();
   	case expr_div(_,_): return tint();
	case expr_add(_,_): return tint();
	case expr_subtr(_,_): return tint();
	case expr_less(_,_): return tbool();
	case expr_leq(_,_): return tbool();
	case expr_greater(_,_): return tbool();
	case expr_geq(_,_): return tbool();
	case expr_eq(_,_): return tbool();
	case expr_neq(_,_): return tbool();
	case expr_and(_,_): return tbool();
	case expr_or(_,_): return tbool();
  }
  return tunknown(); 
}

Type getType(\type(str val)) {
	switch (val) {
		case "integer": return tint();
		case "boolean": return tbool();
		case "string": return tstr();
		default: return tunknown();
	}
}

str toStr(Type t) {
	switch (t) {
		case tint() : return "integer";
		case tstr() : return "string";
		case tbool() : return "bool";
		default : return "unknown";
	}
}
 
 

