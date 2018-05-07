use "ast.sml";
use "tokenizer.sml";

fun isIdExp (EXP_ID _) = true
|	isIdExp _ = false;

fun isId (TK_ID _) = true
  | isId _ = false;

fun isNum (TK_NUM _) = true
  | isNum _ = false;

fun isString (TK_STRING _) = true
  | isString _ = false;

fun getId (TK_ID x) = x;

fun getNum (TK_NUM x) = x;

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
           then Int.toString (getNum token)
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

fun getMultiOperator (TK_TIMES) = BOP_TIMES
|	getMultiOperator (TK_DIVIDE) = BOP_DIVIDE
|	getMultiOperator (TK_MOD) = BOP_MOD;

fun getAddOperator (TK_PLUS) = BOP_PLUS
|	getAddOperator (TK_MINUS) = BOP_MINUS;

fun getEqOperator (TK_EQ) = BOP_EQ
|	getEqOperator (TK_NE) = BOP_NE;

fun getRelOperator (TK_GT) = BOP_GT
|	getRelOperator (TK_GE) = BOP_GE
| 	getRelOperator (TK_LT) = BOP_LT
|	getRelOperator (TK_LE) = BOP_LE;

fun errorOut message =
	(TextIO.output (TextIO.stdErr, message);
	OS.Process.exit OS.Process.failure);

fun parsePrimaryExp [] = errorOut "Expected beginning of expression, no token found\n"
|	parsePrimaryExp tokens =
	if hd tokens = TK_LPAREN
	then
		let
			val (tokens1, exp) = parseExpression (tl tokens);
			val tokens2 = if not (null tokens1) andalso (hd tokens1) = TK_RPAREN then tl tokens1 else errorOut "Expected ) at end of primary expression\n";
		in
			(tokens2, exp)
		end
	else
		case (hd tokens) of
			TK_TRUE => (tl tokens, EXP_TRUE)
		|	TK_FALSE => (tl tokens, EXP_FALSE)
		|	TK_UNDEFINED => (tl tokens, EXP_UNDEFINED)
		|	_ => if isNum (hd tokens)
				 then (tl tokens, EXP_NUM (getNum (hd tokens)))
				 else if isId (hd tokens)
				 	  then (tl tokens, EXP_ID (getId (hd tokens)))
				 	  else if isString (hd tokens)
				 	  	   then (tl tokens, EXP_STRING (getString (hd tokens)))
				 	  	   else errorOut ("Found '" ^ tokenToString (hd tokens) ^ "', expected primary expression\n")

and parseUnaryExp tokens =
	if not (null tokens) andalso isUnaryOp (hd tokens)
	then
		let
			val unaryOp = if (hd tokens) = TK_NOT then UOP_NOT else UOP_MINUS;
			val (tokens1, exp) = parsePrimaryExp (tl tokens);
		in
			(tokens1, EXP_UNARY {opr = unaryOp, opnd = exp})
		end
	else parsePrimaryExp tokens

and parseMultiExp_Helper tokens leftExp =
	let
		val oper = getMultiOperator (hd tokens)
		val (tokens1, rightExp) = parseUnaryExp (tl tokens);
	in
		if not (null tokens1) andalso isMultOp (hd tokens1)
		then parseMultiExp_Helper tokens1 (EXP_BINARY {opr = oper, lft = leftExp, rht = rightExp})
		else (tokens1, (EXP_BINARY {opr = oper, lft = leftExp, rht = rightExp}))
	end

and parseMultiExp tokens =
	let
		val (tokens1, exp) = parseUnaryExp tokens;
	in
		if not (null tokens1) andalso isMultOp (hd tokens1)
		then parseMultiExp_Helper tokens1 exp
		else (tokens1, exp)
	end

and parseAddExp_Helper tokens leftExp =
	let
		val oper = getAddOperator (hd tokens);
		val (tokens1, rightExp) = parseMultiExp (tl tokens);
	in
		if not (null tokens1) andalso isAddOp (hd tokens1)
		then parseAddExp_Helper tokens1 (EXP_BINARY {opr = oper, lft = leftExp, rht = rightExp})
		else (tokens1, (EXP_BINARY {opr = oper, lft = leftExp, rht = rightExp}))
	end

and parseAddExp tokens =
	let
		val (tokens1, exp) = parseMultiExp tokens;
	in
		if not (null tokens1) andalso isAddOp (hd tokens1)
		then parseAddExp_Helper tokens1 exp
		else (tokens1, exp)
	end

and parseRelExp_Helper tokens leftExp = 
	let
		val oper = getRelOperator (hd tokens)
		val (tokens1, rightExp) = parseAddExp (tl tokens);
	in
		if not (null tokens1) andalso isRelOp (hd tokens1)
		then parseRelExp_Helper tokens1 (EXP_BINARY {opr = oper, lft = leftExp, rht = rightExp})
		else (tokens1, (EXP_BINARY {opr = oper, lft = leftExp, rht = rightExp}))
	end

and parseRelExp tokens = 
	let
		val (tokens1, exp) = parseAddExp tokens;
	in
		if not (null tokens1) andalso isRelOp (hd tokens1)
		then parseRelExp_Helper tokens1 exp
		else (tokens1, exp)
	end

and parseEqExp_Helper tokens leftExp =
	let
		val oper = getEqOperator (hd tokens)
		val (tokens1, rightExp) = parseRelExp (tl tokens);
	in
		if not (null tokens1) andalso isEqOp (hd tokens1)
		then parseEqExp_Helper tokens1 (EXP_BINARY {opr = oper, lft = leftExp, rht = rightExp})
		else (tokens1, (EXP_BINARY {opr = oper, lft = leftExp, rht = rightExp}))
	end

and parseEqExp tokens =
	let
		val (tokens1, exp) = parseRelExp tokens;
	in
		if not (null tokens1) andalso isEqOp (hd tokens1)
		then parseEqExp_Helper tokens1 exp
		else (tokens1, exp)
	end

and parseLogicalAndExp_Helper tokens leftExp =
	let
		val (tokens1, rightExp) = parseEqExp tokens;
	in
		if not (null tokens1) andalso (hd tokens1) = TK_AND
		then parseLogicalAndExp_Helper (tl tokens1) (EXP_BINARY {opr = BOP_AND, lft = leftExp, rht = rightExp})
		else (tokens1, (EXP_BINARY {opr = BOP_AND, lft = leftExp, rht = rightExp}))
	end

and parseLogicalAndExp tokens =
	let
		val (tokens1, exp) = parseEqExp tokens;
	in
		if not (null tokens1) andalso (hd tokens1) = TK_AND
		then parseLogicalAndExp_Helper (tl tokens1) exp
		else (tokens1, exp)
	end

and parseLogicalOrExp_Helper tokens leftExp =
	let
		val (tokens1, rightExp) = parseLogicalAndExp tokens;
	in
		if not (null tokens1) andalso (hd tokens1) = TK_OR
		then parseLogicalOrExp_Helper (tl tokens1) (EXP_BINARY {opr = BOP_OR, lft = leftExp, rht = rightExp})
		else (tokens1, (EXP_BINARY {opr = BOP_OR, lft = leftExp, rht = rightExp}))
	end

and parseLogicalOrExp tokens =
	let
		val (tokens1, exp) = parseLogicalAndExp tokens;
	in
		if not (null tokens1) andalso (hd tokens1) = TK_OR
		then parseLogicalOrExp_Helper (tl tokens1) exp
		else (tokens1, exp)
	end

and parseConditionalExpression tokens =
	let
		val (tokens1, guardExp) = parseLogicalOrExp tokens;
	in
		if not (null tokens1) andalso (hd tokens1) = TK_QUESTION
		then
			let
				val (tokens2, thenExp) = parseExpression (tl tokens1);
				val tokens3 = if not (null tokens2) andalso (hd tokens2) = TK_COLON then tl tokens2 else errorOut "Expected : in conditional expression\n";
				val (tokens4, elseExp) = parseExpression tokens3;
			in
				(tokens4, EXP_COND {guard = guardExp, thenExp = thenExp, elseExp = elseExp})
			end
		else (tokens1, guardExp)
	end

and parseExpression tokens =
	let
		val (tokens1, leftExp) = parseConditionalExpression tokens;
	in
		if not (null tokens1) andalso (hd tokens1) = TK_ASSIGN
		then
			let
				val (tokens2, rightExp) = parseExpression (tl tokens1);
			in
				if not (isIdExp leftExp)
				then errorOut "Cannot assign expression to non-identifier\n"
				else (tokens2, EXP_ASSIGN {lhs = leftExp, rhs = rightExp})
			end
		else (tokens1, leftExp)
	end

and parseExpressionStatement (first::rest) =
	let
		val (tokens1, exp) = parseExpression (first::rest);
		val tokens2 = if not (null tokens1) andalso (hd tokens1) = TK_SEMI then tl tokens1 else errorOut "Expected ; at end of expression statement\n";
	in
		(tokens2, ST_EXP {exp = exp})
	end

and parseVarDecs [] varList = errorOut "expected 'identifier'\n"
|	parseVarDecs (first::rest) varList =
	let
		val tokens1 = if not (null rest) andalso first = TK_VAR then rest else errorOut "expected 'var'\n";
		val (tokens2, id) = if not (null tokens1) andalso isId (hd tokens1) then ((tl tokens1), getId (hd tokens1)) else errorOut "expected 'identifier'\n";
		val tokens3 = if not (null tokens2) andalso (hd tokens2) = TK_SEMI
				then tl tokens2 else errorOut "expected ';'\n";
	in
		if not (null tokens3) andalso (hd tokens3) = TK_VAR
		then parseVarDecs tokens3 (varList @ [id])
		else (tokens3, varList @ [id])
	end

and parsePrintStatement (first::rest) =
	let
		val tokens1 = if first = TK_PRINT then rest else errorOut "expected 'print' at beginning of print statement\n";
		val (tokens2, exp) = parseExpression tokens1;
		val tokens3 = if not (null tokens2) andalso (hd tokens2) = TK_SEMI then tl tokens2 else errorOut "expected ';' at end of print statement\n";
	in
		(tokens3, ST_PRINT {exp = exp})
	end

and parseIfStatement (first::rest) =
	let
		val tokens1 = if not (null rest) andalso first = TK_IF then rest else errorOut "expected if at start of if statement\n";
		val tokens2 = if not (null tokens1) andalso (hd tokens1) = TK_LPAREN then tl tokens1 else errorOut "expected ( in if statement\n";
		val (tokens3, guardExp) = parseExpression tokens2;
		val tokens4 = if not (null tokens3) andalso (hd tokens3) = TK_RPAREN then tl tokens3 else errorOut "expected ) in if statement\n";
		val (tokens5, thenSt) = parseBlockStatement tokens4;
		val (tokens6, elseSt) = if not (null tokens5) andalso hd tokens5 = TK_ELSE then parseBlockStatement (tl tokens5) else (tokens5, ST_BLOCK {stmts = []});
	in
		(tokens6, ST_IF {guard = guardExp, th = thenSt, el = elseSt})
	end

and parseWhileStatement (first::rest) =
	let
		val tokens1 = if not (null rest) andalso first = TK_WHILE then rest else errorOut "expected while at start of while statement\n";
		val tokens2 = if not (null tokens1) andalso (hd tokens1) = TK_LPAREN then tl tokens1 else errorOut "expected ( in while statement\n";
		val (tokens3, guardExp) = parseExpression tokens2;
		val tokens4 = if not (null tokens3) andalso (hd tokens3) = TK_RPAREN then tl tokens3 else errorOut "expected ) in while statement\n";
		val (tokens5, bodySt) = parseBlockStatement tokens4;
	in
		(tokens5, ST_WHILE {guard = guardExp, body = bodySt})
	end

and parseBlockStatement tokens =
	let
		val tokens1 = if not (null tokens) andalso hd tokens = TK_LBRACE then tl tokens else errorOut "expected { in block statement\n";
		val (tokens2, statementList) = if not (null tokens1)
									   then if hd tokens1 = TK_RBRACE
					  				   		then (tokens1, [])
					  				   		else parseMultiStatements tokens1 []
					  				   else errorOut "expected beginning of block statement\n";
		val tokens3 = if not (null tokens2) andalso (hd tokens2) = TK_RBRACE then tl tokens2 else errorOut "expected } in block statement\n";
	in
		(tokens3, ST_BLOCK {stmts = statementList})
	end
(*returns (tokens, statement)*)
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
	|	TK_VAR => errorOut ("token '" ^ tokenToString (hd tokens) ^ "' after logical end of program\n")
	|	_ => if isId (hd tokens) orelse isNum (hd tokens) orelse isString (hd tokens)
			 then parseExpressionStatement tokens
			 else errorOut ("Invalid start of statement, token: " ^ tokenToString (hd tokens) ^ "\n")

and parseMultiStatements tokens statementList =
	let
		val (tokens1, statement) = parseStatement tokens;
	in
		if null tokens1 orelse (hd tokens1) = TK_RBRACE
		then (tokens1, (statementList @ [statement]))
		else parseMultiStatements tokens1 (statementList @ [statement])
	end

and parseProgram [] = ([], PROGRAM {decls = [], stmts = []})
|	parseProgram tokens =
	let
		val (tokens1, varList) = if (hd tokens) = TK_VAR then parseVarDecs tokens [] else (tokens, []);
	in
		if null tokens1
		then (tokens1, PROGRAM {decls = varList, stmts = []})
		else
			let
				val (tokens2, statementList) = parseMultiStatements tokens1 [];
			in
				(tokens2, PROGRAM {decls = varList, stmts = statementList})
			end
	end

and parse "" = 
	errorOut "Please give name of file to parse\n"
|	parse fileName = 
	let
		val fstr = TextIO.openIn fileName;
		val tokens = tokenize (TextIO.inputAll fstr);
		val (rest, program) = parseProgram tokens;
	in
		if null rest
		then program
		else errorOut "Extra tokens after expected end of input\n"
	end;