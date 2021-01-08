module Compile

import AST;
import Resolve;
import Eval;
import IO;
import lang::html5::DOM; // see standard library

/*
 * Implement a compiler for QL to HTML and Javascript
 *
 * - assume the form is type- and name-correct
 * - separate the compiler in two parts form2html and form2js producing 2 files
 * - use string templates to generate Javascript
 * - use the HTML5Node type and the `str toString(HTML5Node x)` function to format to string
 * - use any client web framework (e.g. Vue, React, jQuery, whatever) you like for event handling
 * - map booleans to checkboxes, strings to textfields, ints to numeric text fields
 * - be sure to generate uneditable widgets for computed questions!
 * - if needed, use the name analysis to link uses to definitions
 */
map[str name, AType tp] questTypes;

void compile(AForm f) {
	questTypes = (q.id.name : q.tp | /AQuestion q <- f);
  	writeFile(f.src[extension="js"].top, form2js(f));
  	writeFile(f.src[extension="html"].top, toString(form2html(f)));
  	writeFile(f.src[extension="css"].top, css());
}

HTML5Node form2html(AForm f) {
	return html(title("<f.name>"),
				meta(charset("UTF-8")),
				form([ block2html(b), br() | ABlock b <- f.blocks]),
  				script(src(f.src[extension="js"].file)),
  				link(\rel("stylesheet"), \type("text/css"), 
  					 href(f.src[extension="css"].file))
  			 );
}

HTML5Node block2html(ABlock b) {
	switch(b) {
		case block_basic(AQuestion q):
			return div(	
						id(b.src),
						label(\for(q.src), q.label.name),
						input(
							  \type(htmlType(q.tp)), 
							  id(q.id.name),
							  onchange("eval()")
							 )
					  );
		case block_init(AQuestion q, _):
			return div(	
						id(b.src),
						label(\for(q.src), q.label.name),
						input(\type(htmlType(q.tp)), id(q.id.name), disabled(""))
					  );
		case block_list(list[ABlock] blocks):
			return div( 
						[
							id(b.src),
							*[block2html(bk) | ABlock bk <- blocks]
						]
					  );
		case block_if(_, ABlock bt):
			return div( 
						id(b.src),
						addAttr(block2html(bt), class("collapsible"))
					  );
		case block_ifelse(_, ABlock bt, ABlock bf):
			return div(	
						id(b.src),
						addAttr(block2html(bt), class("collapsible")),
						addAttr(block2html(bf), class("collapsible"))
					  );
		default: throw "Unknown block <b>";
	}
}

HTML5Node addAttr(HTML5Node \node, HTML5Attr attr){
	\node.kids += [attr];
	return \node; 
}

str htmlType(AType tp) {
	switch (tp.val) {
		case "integer": return "number";
		case "boolean": return "checkbox";
		case "string": return "text"; 
		default: throw "Unknown type <tp>";
	}
}


str form2js(AForm f) {
	VEnv venv = initialEnv(f); 
	return 	"
			'<for (str k <- venv){>
  			'<elemVal(k)> = <val2js(venv[k])>;<}>
  			'
  			'function eval()
  			'{
  			' 	let changed;
  			'	do
  			'	{ 
  			'		changed = false;
  			'		let prev;
  			'		<for (ABlock b <- f.blocks){>
  			'		<block2js(b)><}>
  			'	}while(changed);
  			'};
  			'
  			'eval();
  			";
}

str block2js(ABlock b){
	switch(b) {
		case block_basic(_): return "";
		case block_init(AQuestion q, AExpr e):
			return 	"prev = <elemVal(q.id.name)>;
					'<elemVal(q.id.name)> = <expr2js(e)>;
					'changed = prev != <elemVal(q.id.name)> ? true : changed;
					";
		case block_list(list[ABlock] bks):
			return 	"{<for (bk <- bks){>
					'<block2js(bk)><}>
					'}
					";
		case block_if(AExpr e, ABlock bt):
			return 	"if (<expr2js(e)>)
					'{	
					'	<elemStyle("<bt.src>", "display", "block")>;
					'	<block2js(bt)>
					'} 
					'else
					'	<elemStyle("<bt.src>", "display", "none")>;
					";
		case block_ifelse(AExpr e, ABlock bt, ABlock bf):
			return 	"if (<expr2js(e)>)
					'{	
					'	<elemStyle("<bt.src>", "display", "block")>;
					'	<elemStyle("<bf.src>", "display", "none")>;
					'	<block2js(bt)>
					'}
					'else
					'{	
					'	<elemStyle("<bt.src>", "display", "none")>;
					'	<elemStyle("<bf.src>", "display", "block")>;
					'	<block2js(bf)>
					'}
					";
		default: throw "Unknown block <b>";
	}
}

str expr2js(AExpr e){
	switch(e) {
		case ref(AId id): return "<getElemVal(id.name)>";
		case \str(str val): return "<val>";
		case \int(int val): return "<val>";
		case \bool(bool val): return "<val>";
		case expr_paranth(AExpr expr): return "(<expr2js(expr)>)";
		case expr_neg(AExpr expr): return "!(<expr2js(expr)>)";
		case expr_mult(AExpr lhs, AExpr rhs): return "(<expr2js(lhs)> * <expr2js(rhs)>)";
		case expr_div(AExpr lhs, AExpr rhs): return "(<expr2js(lhs)> / <expr2js(rhs)>)";
		case expr_add(AExpr lhs, AExpr rhs): return "(<expr2js(lhs)> + <expr2js(rhs)>)";
		case expr_subtr(AExpr lhs, AExpr rhs): return "(<expr2js(lhs)> - <expr2js(rhs)>)";
		case expr_less(AExpr lhs, AExpr rhs): return "(<expr2js(lhs)> \< <expr2js(rhs)>)";
		case expr_leq(AExpr lhs, AExpr rhs): return "(<expr2js(lhs)> \<= <expr2js(rhs)>)";
		case expr_greater(AExpr lhs, AExpr rhs): return "(<expr2js(lhs)> \> <expr2js(rhs)>)";
		case expr_geq(AExpr lhs, AExpr rhs): return "(<expr2js(lhs)> \>= <expr2js(rhs)>)";
		case expr_eq(AExpr lhs, AExpr rhs): return "(<expr2js(lhs)> == <expr2js(rhs)>)";
		case expr_neq(AExpr lhs, AExpr rhs): return "(<expr2js(lhs)> != <expr2js(rhs)>)";
		case expr_and(AExpr lhs, AExpr rhs): return "(<expr2js(lhs)> && <expr2js(rhs)>)";
		case expr_or(AExpr lhs, AExpr rhs): return "(<expr2js(lhs)> || <expr2js(rhs)>)";
		default: throw "Unknown expression <e>";
	}
}

str elemVal(str id)
	= "document.getElementById(\"<id>\").<questTypes[id].val == "boolean" ? "checked" : "value">";

str elemStyle(str id, str prop, str val) 
	= "document.getElementById(\"<id>\").style.<prop> = \"<val>\"";

str getElemVal(str id) 
	= questTypes[id].val == "integer" ? "parseInt(<elemVal(id)>)" : elemVal(id); 


int val2js(vint(int v)) = v; 
bool val2js(vbool(bool v)) = v; 
str val2js(vstr(str v)) = v; 

str css() {
	return 	".collapsible {
			'	border: 2px solid blue;
  			'	border-radius: 5px;
  			'	overflow: hidden;	
  			'	display: none;
  			'	padding: 5px;
			'}
			'
			'input {
			'	display: flex;
			'	margin: auto;
			'}
			'
			'form {
			'	text-align:center;
			'	width: 33%;
			'	margin: auto;
			'	border: 2px solid green;
			' 	border-radius: 5px;
			'	padding: 10px;
			'}";
}
