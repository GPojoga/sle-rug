module Compile

import AST;
import Resolve;
import Eval;
import IO;
import lang::html5::DOM; // see standard library
import List;

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

HTML5Node form2html(AForm f)
	= html(	title("<f.name>"),
			meta(charset("UTF-8")),
			form([ block2html(b) | ABlock b <- f.blocks]),
  			script(src(f.src[extension="js"].file)),
  			link(\rel("stylesheet"), \type("text/css"), href(f.src[extension="css"].file)));

HTML5Node block2html(b:block_basic(AQuestion q)) {
	if (q.tp.val == "boolean")
		return addClass(div(id(b.src),
							label(\for(q.src), q.label.name), 
							input(\type("text"), id(q.id.name), 
								  onchange("eval(); updateBtns(this.value, \'<q.id.name>__y__\', \'<q.id.name>__n__\')"), 
								  html5attr("style","display: none;")),
							button("Yes", q.id.name, "<q.id.name>__y__", "setBool(true, \'<q.id.name>\')"),
							button("No", q.id.name, "<q.id.name>__n__", "setBool(false, \'<q.id.name>\')")),
						"question"); 
	else
		return addClass(div(id(b.src),
							label(\for(q.src), q.label.name), 
							input(\type(htmlType(q.tp.val)), id(q.id.name), onchange("eval()"))),
						"question"); 
}

HTML5Node block2html(b:block_init(AQuestion q, _)) {
	if (q.tp.val == "boolean")
		return addClass(div(id(b.src),
							label(\for(q.src), q.label.name), 
							input(\type("text"), id(q.id.name), 
								  onchange("eval(); updateBtns(this.value, \'<q.id.name>__y__\', \'<q.id.name>__n__\')"), 
								  html5attr("style","display: none;")),
							addAttr(button("Yes", q.id.name, "<q.id.name>__y__","setBool(true, \'<q.id.name>\')"),
									disabled("")),						
							addAttr(button("No", q.id.name, "<q.id.name>__n__", "setBool(false, \'<q.id.name>\')"),
									disabled(""))),
						"question"); 
	else
		return addClass(div(id(b.src),
							label(\for(q.src), q.label.name), 
							input(\type(htmlType(q.tp.val)), id(q.id.name), onchange("eval()"), disabled(""))),
						"question"); 
}

HTML5Node button(str name, str qstname, str bid, str onclck) 
	= addClass(input(\type("button"), 
					 id(bid),
					\value(name), 
					onclick("<onclck>")),
				"inactive");

HTML5Node block2html(b:block_list(list[ABlock] blocks))
	= div([id(b.src), *[block2html(bk) | ABlock bk <- blocks]]);
			
HTML5Node block2html(b:block_if(_, ABlock bt))
	= div(id(b.src),addClass(block2html(bt), "collapsible"));
	
HTML5Node block2html(b:block_ifelse(_, ABlock bt, ABlock bf))
	= div(	id(b.src), 
			addClass(block2html(bt), "collapsible"), 
			addClass(block2html(bf), "collapsible"));

HTML5Node addAttr(HTML5Node \node, HTML5Attr attr){
	\node.kids += [attr];
	return \node;
}

HTML5Node addClass(HTML5Node \node, str cs){
	for (int i <- [0..size(\node.kids)])
		if (html5attr("class", value v) := \node.kids[i]){
			\node.kids[i] = html5attr("class", "<v> <cs>");
			return \node;
		}
	\node.kids += [class(cs)];
	return \node; 
}

str htmlType("integer")  = "number";
str htmlType("string") = "text";

str form2js(AForm f)
	= 	"
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
  		'function updateBtns(val, byes, bno)
  		'{	
  		'	if (val == \"true\") {
  		'		document.getElementById(byes).className = \"active\";
  		'		document.getElementById(bno).className = \"inactive\";
  		'	}
  		'	else
  		'	{
  		'		document.getElementById(byes).className = \"inactive\";
  		'		document.getElementById(bno).className = \"active\";	
  		'	}
  		'}
  		'
  		'function setBool(val, target) {
  		'	document.getElementById(target).value = val;
  		'	document.getElementById(target).dispatchEvent(new Event(\'change\'));
  		'}
  		'eval();
  		";

str block2js(block_basic(_)) = "";

str block2js(block_init(AQuestion q, AExpr e))
	= 	"if (<toStrSep(["true", *["<elemVal(x.name)>!=\"\"" | /AId x <- e]], " && ")>)
		'{
		'	prev = <elemVal(q.id.name)>;
		'	<elemVal(q.id.name)> = <expr2js(e)>;
		'	<if (q.tp.val == "boolean") {>updateBtns(<elemVal(q.id.name)>, \'<q.id.name>__y__\', \'<q.id.name>__n__\'); <}>
		'	changed = prev != <elemVal(q.id.name)> ? true : changed;
		'}
		";
		
str block2js(block_list(list[ABlock] bks))
	= 	"{<for (bk <- bks){>
		'<block2js(bk)><}>
		'}
		";
		
str block2js(block_if(AExpr e, ABlock bt))
	= 	"if (<toStrSep(["true", *["<elemVal(x.name)>!=\"\"" | /AId x <- e]], " && ")>)
		'{
		'	if (<expr2js(e)>)
		'	{	
		'		<elemStyle("<bt.src>", "display", "block")>;
		'		<block2js(bt)>
		'	} 
		'	else
		'		<elemStyle("<bt.src>", "display", "none")>;
		'}
		";
		
str block2js(block_ifelse(AExpr e, ABlock bt, ABlock bf))
	=	"if (<toStrSep(["true", *["<elemVal(x.name)>!=\"\"" | /AId x <- e]], " && ")>)
		'{
		'	if (<expr2js(e)>)
		'	{	
		'		<elemStyle("<bt.src>", "display", "block")>;
		'		<elemStyle("<bf.src>", "display", "none")>;
		'		<block2js(bt)>
		'	}
		'	else
		'	{	
		'		<elemStyle("<bt.src>", "display", "none")>;
		'		<elemStyle("<bf.src>", "display", "block")>;
		'		<block2js(bf)>
		'	}
		'}
		";

str expr2js(ref(AId id)) = "<getElemVal(id.name)>";
str expr2js(\str(str val)) = "<val>";
str expr2js(\int(int val)) = "<val>";
str expr2js(\bool(bool val)) = "<val>";
str expr2js(expr_paranth(AExpr expr)) = "(<expr2js(expr)>)";
str expr2js(expr_neg(AExpr expr)) = "!(<expr2js(expr)>)";
str expr2js(expr_mult(AExpr lhs, AExpr rhs)) = "(<expr2js(lhs)> * <expr2js(rhs)>)";
str expr2js(expr_div(AExpr lhs, AExpr rhs)) = "(<expr2js(lhs)> / <expr2js(rhs)>)";
str expr2js(expr_add(AExpr lhs, AExpr rhs)) = "(<expr2js(lhs)> + <expr2js(rhs)>)";
str expr2js(expr_subtr(AExpr lhs, AExpr rhs)) = "(<expr2js(lhs)> - <expr2js(rhs)>)";
str expr2js(expr_less(AExpr lhs, AExpr rhs)) = "(<expr2js(lhs)> \< <expr2js(rhs)>)";
str expr2js(expr_leq(AExpr lhs, AExpr rhs)) = "(<expr2js(lhs)> \<= <expr2js(rhs)>)";
str expr2js(expr_greater(AExpr lhs, AExpr rhs)) = "(<expr2js(lhs)> \> <expr2js(rhs)>)";
str expr2js(expr_geq(AExpr lhs, AExpr rhs)) = "(<expr2js(lhs)> \>= <expr2js(rhs)>)"; 
str expr2js(expr_eq(AExpr lhs, AExpr rhs)) = "(<expr2js(lhs)> == <expr2js(rhs)>)"; 
str expr2js(expr_neq(AExpr lhs, AExpr rhs)) = "(<expr2js(lhs)> != <expr2js(rhs)>)";
str expr2js(expr_and(AExpr lhs, AExpr rhs)) = "(<expr2js(lhs)> && <expr2js(rhs)>)";
str expr2js(expr_or(AExpr lhs, AExpr rhs)) = "(<expr2js(lhs)> || <expr2js(rhs)>)";


str elemVal(str id)
	= "document.getElementById(\"<id>\").value";

str elemStyle(str id, str prop, str val) 
	= "document.getElementById(\"<id>\").style.<prop> = \"<val>\"";

str getElemVal(str id) 
	= questTypes[id].val == "integer" ? "parseInt(<elemVal(id)>)" :
	  questTypes[id].val == "boolean" ? "(<elemVal(id)> == \"true\")" : elemVal(id); 


int val2js(vint(int v)) = v; 
bool val2js(vbool(bool v)) = v; 
str val2js(vstr(str v)) = v; 

str toStrSep(list[str] lst, str sep) {
	if (size(lst) == 0) return "";
	return "<lst[0]><for (int i <- [1..size(lst)]){><sep><lst[i]><}>";
}

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
			'input:disabled {
			'	color: rgb(124, 110, 108);
			'}
			'
			'form {
			'	text-align:center;
			'	width: 33%;
			'	margin: auto;
			'	border: 2px solid green;
			' 	border-radius: 5px;
			'	padding: 10px;
			'}
			'
			'.question {
			'	padding: 5px;
			'}
			'
			'.inactive {
    		'	border-radius: 10px;
    		'	display: inline;
    		'	background-color: rgb(165, 162, 157); 
    		'	width: 25%;
    		'	color: white;
    		'	font-weight: bold;
    		'	margin: auto;
			'}
			'
			'label {
			'	display: block;
			'}
			'.active {
    		'	border-radius: 10px;
    		'	display: inline;
    		'	background-color: rgb(87, 112, 255); 
    		'	width: 25%;
    		'	color: white;
    		'	font-weight: bold;
    		'	margin: auto;
			'}";
}
