use "ast.sml";
use "tokenizer.sml";

fun isId (TK_ID _) = true
  | isId _ = false;

fun isNum (TK_NUM _) = true
  | isNum _ = false;

fun isString (TK_STRING _) = true
  | isString _ = false;

fun getId (TK_ID x) = x;

fun getNum (TK_NUM x) = Int.toString x;

fun getString (TK_STRING x) = x;

fun tokenToString token =
   case token of
      TK_LBRACE => "{"
   | TK_RBRACE => "}"
   | TK_LPAREN => "("
   | TK_RPAREN => ")"
   | TK_COMMA => ","
   | TK_SEMI => ";"
   | TK_QUESTION => "?"
   | TK_COLON => ":"
   | TK_DOT => "."
   | TK_PLUS => "+"
   | TK_MINUS => "-"
   | TK_TIMES => "*"
   | TK_DIVIDE => "/"
   | TK_MOD => "%"
   | TK_AND => "&&"
   | TK_OR => "||"
   | TK_ASSIGN => "="
   | TK_EQ => "=="
   | TK_LT => "<"
   | TK_LE => "<="
   | TK_GT => ">"
   | TK_GE => ">="
   | TK_NOT => "!"
   | TK_NE => "!="
   | TK_ANON => "anon"
   | TK_CLASS => "class"
   | TK_ELSE => "else"
   | TK_EXTENDS => "extends"
   | TK_FALSE => "false"
   | TK_FUN => "fun"
   | TK_IF => "if"
   | TK_NAME => "name"
   | TK_NEW => "new"
   | TK_PRINT => "print"
   | TK_REF => "ref"
   | TK_RETURN => "return"
   | TK_SUPER => "super"
   | TK_THIS => "this"
   | TK_TRUE => "true"
   | TK_UNDEFINED => "undefined"
   | TK_VAR => "var"
   | TK_WHILE => "while"
   |  _ => if isNum token
           then getNum token
           else if isId token
                then getId token
                else if isString token
                     then getString token
                     else "UNIDENTIFIED TOKEN";

fun isEqOp (TK_EQ) = true
|	isEqOp (TK_NE) = true
|	isEqOp _ = false;

fun isRelOp (TK_LT) = true
|	isRelOp (TK_GT) = true
|	isRelOp (TK_LE) = true
|	isRelOp (TK_GE) = true
|	isRelOp _ = false;

fun isAddOp (TK_PLUS) = true
|	isAddOp (TK_MINUS) = true
|	isAddOp _ = false;

fun isMultOp (TK_TIMES) = true
|	isMultOp (TK_DIVIDE) = true
|	isMultOp (TK_MOD) = true
|	isMultOp _ = false;

fun isUnaryOp (TK_NOT) = true
|	isUnaryOp (TK_MINUS) = true
|	isUnaryOp _ = false;

fun errorOut message =
	(TextIO.output (TextIO.stdErr, message);
	OS.Process.exit OS.Process.failure);

fun parsePrimaryExp [] = errorOut "Expected beginning of expression, no token found\n"
|	parsePrimaryExp tokens =
	if hd tokens = TK_LPAREN
	then
		let
			val tokens1 = parseExpression (tl tokens);
			val tokens2 = if not (null tokens1) andalso (hd tokens1) = TK_RPAREN then tl tokens1 else errorOut "Expected ) at end of primary expression\n";
		in
			tokens2
		end
	else
		case (hd tokens) of
			TK_TRUE => tl tokens
		|	TK_FALSE => tl tokens
		|	TK_UNDEFINED => tl tokens
		|	_ => if isNum (hd tokens)
				 then tl tokens
				 else if isId (hd tokens)
				 	  then tl tokens
				 	  else if isString (hd tokens)
				 	  	   then tl tokens
				 	  	   else errorOut "Invalid expression\n"
and parseUnaryExp tokens =
	if not (null tokens) andalso isUnaryOp (hd tokens)
	then parsePrimaryExp  (tl tokens)
	else parsePrimaryExp tokens

and parseMultiExp_Helper tokens =
	let
		val tokens1 = parseUnaryExp tokens;
	in
		if not (null tokens1) andalso isMultOp (hd tokens1)
		then parseMultiExp_Helper (tl tokens1)
		else tokens1
	end

and parseMultiExp tokens =
	let
		val tokens1 = parseUnaryExp tokens;
	in
		if not (null tokens1) andalso isMultOp (hd tokens1)
		then parseMultiExp_Helper (tl tokens1)
		else tokens1
	end

and parseAddExp_Helper tokens =
	let
		val tokens1 = parseMultiExp tokens;
	in
		if not (null tokens1) andalso isAddOp (hd tokens1)
		then parseAddExp_Helper (tl tokens1)
		else tokens1
	end

and parseAddExp tokens =
	let
		val tokens1 = parseMultiExp tokens;
	in
		if not (null tokens1) andalso isAddOp (hd tokens1)
		then parseAddExp_Helper (tl tokens1)
		else tokens1
	end

and parseRelExp_Helper tokens = 
	let
		val tokens1 = parseAddExp tokens;
	in
		if not (null tokens1) andalso isRelOp (hd tokens1)
		then parseRelExp_Helper (tl tokens1)
		else tokens1
	end

and parseRelExp tokens = 
	let
		val tokens1 = parseAddExp tokens;
	in
		if not (null tokens1) andalso isRelOp (hd tokens1)
		then parseRelExp_Helper (tl tokens1)
		else tokens1
	end

and parseEqExp_Helper tokens =
	let
		val tokens1 = parseRelExp tokens;
	in
		if not (null tokens1) andalso isEqOp (hd tokens1)
		then parseEqExp_Helper (tl tokens1)
		else tokens1
	end

and parseEqExp tokens =
	let
		val tokens1 = parseRelExp tokens;
	in
		if not (null tokens1) andalso isEqOp (hd tokens1)
		then parseEqExp_Helper (tl tokens1)
		else tokens1
	end

and parseLogicalAndExp_Helper tokens =
	let
		val tokens1 = parseEqExp tokens;
	in
		if not (null tokens1) andalso (hd tokens1) = TK_AND
		then parseLogicalAndExp_Helper (tl tokens1)
		else tokens1
	end

and parseLogicalAndExp tokens =
	let
		val tokens1 = parseEqExp tokens;
	in
		if not (null tokens1) andalso (hd tokens1) = TK_AND
		then parseLogicalAndExp_Helper (tl tokens1)
		else tokens1
	end

and parseLogicalOrExp_Helper tokens =
	let
		val tokens1 = parseLogicalAndExp tokens;
	in
		if not (null tokens1) andalso (hd tokens1) = TK_OR
		then parseLogicalOrExp_Helper (tl tokens1)
		else tokens1
	end

and parseLogicalOrExp tokens =
	let
		val tokens1 = parseLogicalAndExp tokens;
	in
		if not (null tokens1) andalso (hd tokens1) = TK_OR
		then parseLogicalOrExp_Helper (tl tokens1)
		else tokens1
	end

and parseConditionalExpression tokens =
	let
		val tokens1 = parseLogicalOrExp tokens;
	in
		if not (null tokens1) andalso (hd tokens1) = TK_QUESTION
		then
			let
				val tokens2 = parseExpression (tl tokens1);
				val tokens3 = if not (null tokens2) andalso (hd tokens2) = TK_COLON then tl tokens2 else errorOut "Expected : in conditional expression\n";
				val tokens4 = parseExpression tokens3;
			in
				tokens4
			end
		else tokens1
	end

and parseExpression tokens =
	let
		val tokens1 = parseConditionalExpression tokens;
	in
		if not (null tokens1) andalso (hd tokens1) = TK_ASSIGN
		then parseExpression (tl tokens1)
		else tokens1
	end

and parseExpressionStatement (first::rest) =
	let
		val tokens1 = parseExpression (first::rest);
		val tokens2 = if not (null tokens1) andalso (hd tokens1) = TK_SEMI then tl tokens1 else errorOut "Expected ; at end of expression statement\n";
	in
		tokens2
	end

and parseVarDecs [] = errorOut "Identifier expected in variable declaration\n"
|	parseVarDecs (first::rest) =
	let
		val tokens1 = if not (null rest) andalso first = TK_VAR then rest else errorOut "var expected at beginning of variable declaration\n";
		val tokens2 = if not (null tokens1) andalso isId (hd tokens1) then (tl tokens1) else errorOut "Identifier expected in variable declaration\n";
		val tokens3 = if not (null tokens2) andalso (hd tokens2) = TK_SEMI
				then tl tokens2 else errorOut "; expected at end of variable declaration\n";
	in
		if not (null tokens3) andalso (hd tokens3) = TK_VAR
		then parseVarDecs tokens3
		else tokens3
	end

and parsePrintStatement (first::rest) =
	let
		val tokens1 = if first = TK_PRINT then rest else errorOut "Expected 'print' at beginning of print statement\n";
		val tokens2 = parseExpression tokens1;
		val tokens3 = if not (null tokens2) andalso (hd tokens2) = TK_SEMI then tl tokens2 else errorOut "Expected ';' at end of print statement\n";
	in
		tokens3
	end

and parseIfStatement (first::rest) =
	let
		val tokens1 = if not (null rest) andalso first = TK_IF then rest else errorOut "Expected if at start of if statement\n";
		val tokens2 = if not (null tokens1) andalso (hd tokens1) = TK_LPAREN then tl tokens1 else errorOut "Expected ( in if statement\n";
		val tokens3 = parseExpression tokens2;
		val tokens4 = if not (null tokens3) andalso hd tokens3 = TK_RPAREN then tl tokens3 else errorOut "Expected ) in if statement\n";
		val tokens5 = parseBlockStatement tokens4;
		val tokens6 = if not (null tokens5) andalso hd tokens5 = TK_ELSE then parseBlockStatement (tl tokens5) else tokens5;
	in
		tokens6
	end

and parseWhileStatement (first::rest) =
	let
		val tokens1 = if not (null rest) andalso first = TK_WHILE then rest else errorOut "Expected while at start of while statement\n";
		val tokens2 = if not (null tokens1) andalso (hd tokens1) = TK_LPAREN then tl tokens1 else errorOut "Expected ( in while statement\n";
		val tokens3 = parseExpression tokens2;
		val tokens4 = if not (null tokens3) andalso hd tokens3 = TK_RPAREN then tl tokens3 else errorOut "Expected ) in while statement\n";
		val tokens5 = parseBlockStatement tokens4;
	in
		tokens5
	end

and parseBlockStatement tokens =
	let
		val tokens1 = if not (null tokens) andalso hd tokens = TK_LBRACE then tl tokens else errorOut "Expected { in block statement\n";
		val tokens2 = if not (null tokens1) andalso hd tokens1 = TK_RBRACE
					  then tokens1 else parseMultiStatements tokens1;
		val tokens3 = if not (null tokens2) andalso (hd tokens2) = TK_RBRACE then tl tokens2 else errorOut "Expected } in block statement\n";
	in
		tokens3
	end

and parseStatement tokens =
	case (hd tokens) of
		TK_PRINT => parsePrintStatement tokens
	|	TK_IF => parseIfStatement tokens
	|	TK_WHILE => parseWhileStatement tokens
	|	TK_LBRACE => parseBlockStatement tokens
	|	TK_NOT => parseExpressionStatement tokens
	|	TK_MINUS => parseExpressionStatement tokens
	|	TK_LPAREN => parseExpressionStatement tokens
	|	TK_TRUE => parseExpressionStatement tokens
	|	TK_FALSE => parseExpressionStatement tokens
	|	TK_UNDEFINED => parseExpressionStatement tokens
	|	TK_VAR => errorOut "Cannot have variable declaration after statements\n"
	|	_ => if isId (hd tokens) orelse isNum (hd tokens) orelse isString (hd tokens)
			 then parseExpressionStatement tokens
			 else errorOut ("Invalid start of statement, token: " ^ tokenToString (hd tokens) ^ "\n")

and parseMultiStatements tokens =
	let
		val tokens1 = parseStatement tokens;
	in
		if null tokens1 orelse (hd tokens1) = TK_RBRACE
		then tokens1
		else parseMultiStatements tokens1
	end

and parseProgram [] = []
|	parseProgram tokens =
	let
		val tokens1 = if (hd tokens) = TK_VAR then parseVarDecs tokens else tokens;
	in
		if null tokens1
		then tokens1
		else parseMultiStatements tokens1
	end

and parse "" = 
	TextIO.print "Please give name of file to parse\n"
|	parse fileName = 
	let
		val fstr = TextIO.openIn fileName;
		val tokens = tokenize (TextIO.inputAll fstr);
		val rest = parseProgram tokens;
	in
		if null rest
		then TextIO.print ""
		else errorOut "Extra tokens after expected end of input\n"
	end;