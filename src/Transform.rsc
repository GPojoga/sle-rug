module Transform

import ParseTree;
import Syntax;
import Resolve;
import AST;
import Set;
import String;

/* 
 * Transforming QL forms
 */
 
 
/* Normalization:
 *  wrt to the semantics of QL the following
 *     q0: "" int; 
 *     if (a) { 
 *        if (b) { 
 *          q1: "" int; 
 *        } 
 *        q2: "" int; 
 *      }
 *
 *  is equivalent to
 *     if (true) q0: "" int;
 *     if (true && a && b) q1: "" int;
 *     if (true && a) q2: "" int;
 *
 * Write a transformation that performs this flattening transformation.
 *
 */

anno loc Id @ \loc;

AForm flatten(AForm f)
  	= form(f.name, [*flatten(b, \bool(true)) | ABlock b <- f.blocks]); 

list[ABlock] flatten(ABlock b:block_basic(_), AExpr cond) 
	= [block_if(cond, b)];

list[ABlock] flatten(ABlock b:block_init(_,_), AExpr cond) 
	= [block_if(cond, b)];

list[ABlock] flatten(block_list(list[ABlock] bks), AExpr cond)
	= [*flatten(b, cond) | ABlock b <- bks];

list[ABlock] flatten(block_if(AExpr expr, ABlock bt), AExpr cond)
	= flatten(bt, expr_and(cond, expr_paranth(expr)));

list[ABlock] flatten(block_ifelse(AExpr expr, ABlock bt, ABlock bf), AExpr cond)
	= 	flatten(bt, expr_and(cond, expr_paranth(expr))) + 
		flatten(bf, expr_and(cond, expr_neg(expr_paranth(expr))));

/* Rename refactoring:
 *
 * Write a refactoring transformation that consistently renames all occurrences of the same name.
 * Use the results of name resolution to find the equivalence class of a name.
 *
 */
 
start[Form] rename(start[Form] f, loc useOrDef, str newName, UseDef useDef) {
 	loc def = size(useDef[useOrDef]) == 0 ? useOrDef : getFirstFrom(useDef[useOrDef]);
 	set[loc] toRename = {def, *{h | <h, def> <- useDef}};
 	
   	return visit(f) {
   		case Id x => newId(newName, x@\loc)
   			when x@\loc in toRename	
   	} 
} 

Id newId(str name, loc path) { //used to set the location of the new Id 
	Id id = [Id]name;
	path.length = size(name);
	path.end.column = path.begin.column + path.length;
	id@\loc = path;
	return id;
}
 
 
 
 

