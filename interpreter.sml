use "parser.sml";

open HashTable;

exception CannotFindIt;

val hash_fn : string->word = HashString.hashString;
val cmp_fn : string*string->bool = (op =);
val initial_size : int = 101;

datatype interpretValue =
	BOOLEAN of bool
|	STRING of string
|	NUMBER of int
|	ID of string
|	UNDEFINED
;

fun valueIsNum (NUMBER x) = true
|	valueIsNum _ = false;

fun valueIsBool (BOOLEAN x) = true
|	valueIsBool _ = false;

fun valueIsString (STRING x) = true
|	valueIsString _ = false;

fun valueIsId (ID x) = true
|	valueIsId _ = false;

fun valueIsUndefined (UNDEFINED) = true
|	valueIsUndefined _ = false;

fun binOprIsMath (BOP_PLUS) = true (*PLUS is also for strings*)
|	binOprIsMath (BOP_MINUS) = true
|	binOprIsMath (BOP_TIMES) = true
|	binOprIsMath (BOP_DIVIDE) = true
|	binOprIsMath (BOP_MOD) = true
|	binOprIsMath (BOP_LT) = true
|	binOprIsMath (BOP_LE) = true
|	binOprIsMath (BOP_GT) = true
|	binOprIsMath (BOP_GE) = true
|	binOprIsMath _ = false;

fun binOprIsBool (BOP_AND) = true
|	binOprIsBool (BOP_OR) = true
|	binOprIsBool _ = false;

fun binOprIsEq (BOP_EQ) = true
|	binOprIsEq (BOP_NE) = true
|	binOprIsEq _ = false;

fun getNumFromValue (NUMBER x) = x;

fun getStringFromValue (STRING x) = x;

fun getBoolFromValue (BOOLEAN x) = x;

fun getIdFromValue (ID x) = x;

fun isValidExpressionEq_Helper (EXP_BINARY {opr, lft, rht}) =
	let
		val leftValue = interpretExpression lft;
		val rightValue = interpretExpression rht;
	in
		if valueIsBool leftValue andalso valueIsBool rightValue
		then true
		else if valueIsString leftValue andalso valueIsString rightValue
			 then true
			 else if valueIsNum leftValue andalso valueIsNum rightValue
			 	  then true
			 	  else if valueIsUndefined leftValue andalso valueIsUndefined rightValue
			 	  	   then true
			 	  	   else false
	end

(*Checks to see if expression's lft and rht types are correct for operator*)
and isValidExpression (EXP_ASSIGN {lhs, rhs}) =
	true (*TODO*)
|	isValidExpression (EXP_UNARY {opr, opnd}) =
	if opr = UOP_NOT
	then (valueIsBool (interpretExpression opnd))
	else (valueIsNum (interpretExpression opnd))
|	isValidExpression (EXP_BINARY {opr=BOP_EQ, lft, rht}) =
	isValidExpressionEq_Helper (EXP_BINARY {opr=BOP_EQ, lft=lft, rht=rht})
|	isValidExpression (EXP_BINARY {opr=BOP_NE, lft, rht}) =
	isValidExpressionEq_Helper (EXP_BINARY {opr=BOP_NE, lft=lft, rht=rht})
|	isValidExpression (EXP_BINARY {opr, lft, rht}) =
	let
		val leftValue = interpretExpression lft;
		val rightValue = interpretExpression rht;
	in
		if binOprIsBool opr andalso valueIsBool leftValue andalso valueIsBool rightValue
		then true
		else if binOprIsMath opr andalso valueIsNum leftValue andalso valueIsNum rightValue
			 then true
			 else if opr = BOP_PLUS andalso valueIsString leftValue andalso valueIsString rightValue
			 	  then true
			 	  else false
	end

and interpretBinaryExpression (EXP_BINARY {opr, lft, rht}) =
	if isValidExpression (EXP_BINARY {opr=opr, lft=lft, rht=rht})
	then
		if binOprIsBool opr
		then (*Boolean operation*)
			let
				val leftValue = getBoolFromValue (interpretExpression lft);
				val rightValue = getBoolFromValue (interpretExpression rht);
			in
				case opr of
					BOP_AND => if not leftValue then BOOLEAN false else BOOLEAN rightValue
				|	BOP_OR => if leftValue then BOOLEAN true else BOOLEAN rightValue
			end
		else if binOprIsEq opr
			 then (*Eqality operation*)
			 	case opr of
			 		BOP_EQ => BOOLEAN ((interpretExpression lft) = (interpretExpression rht))
			 	|	BOP_NE => BOOLEAN (not ((interpretExpression lft) = (interpretExpression rht)))
			 else if binOprIsMath opr andalso not (valueIsString (interpretExpression lft))
			 	  then (*Math operation*)
			 	  	let
			 	  		val leftValue = getNumFromValue (interpretExpression lft);
			 	  		val rightValue = getNumFromValue (interpretExpression rht);
			 	  	in
			 	  		case opr of
			 	  			BOP_PLUS => NUMBER (leftValue + rightValue)
			 	  		|	BOP_MINUS => NUMBER (leftValue - rightValue)
			 	  		|	BOP_TIMES => NUMBER (leftValue * rightValue)
			 	  		|	BOP_DIVIDE => NUMBER (leftValue div rightValue)
			 	  		|	BOP_MOD => NUMBER (leftValue mod rightValue)
			 	  		|	BOP_LT => BOOLEAN (leftValue < rightValue)
			 	  		|	BOP_LE => BOOLEAN (leftValue <= rightValue)
			 	  		|	BOP_GT => BOOLEAN (leftValue > rightValue)
			 	  		|	BOP_GE => BOOLEAN (leftValue >= rightValue)
			 	  	end
			 	  else if opr = BOP_PLUS
			 	  	   then STRING (getStringFromValue (interpretExpression lft) ^ getStringFromValue (interpretExpression rht))
			 	  	   else errorOut "Error in binary operation\n"
	else errorOut "Bad type in binary expression\n"

and interpretExpression EXP_TRUE = BOOLEAN true
|	interpretExpression EXP_FALSE = BOOLEAN false
|	interpretExpression (EXP_STRING x) = STRING x
|	interpretExpression (EXP_NUM x) = NUMBER x
|	interpretExpression EXP_UNDEFINED = UNDEFINED
|	interpretExpression (EXP_ID x) = UNDEFINED (*TODO*)
|	interpretExpression (EXP_BINARY {opr, lft, rht}) = 
	interpretBinaryExpression (EXP_BINARY {opr=opr, lft=lft, rht=rht})
|	interpretExpression (EXP_UNARY {opr, opnd}) =
	if isValidExpression (EXP_UNARY {opr=opr, opnd=opnd})
	then
		case opr of
			UOP_NOT => BOOLEAN (not (getBoolFromValue (interpretExpression opnd)))
		|	UOP_MINUS => NUMBER (~(getNumFromValue (interpretExpression opnd)))
	else errorOut "Bad type in unary expression\n"
|	interpretExpression (EXP_COND {guard, thenExp, elseExp}) =
	if (interpretExpression guard) = BOOLEAN true
	then interpretExpression thenExp
	else if (interpretExpression guard) = BOOLEAN false
		 then interpretExpression elseExp
		 else errorOut "Guard of conditional expression must be of boolean type\n"
|	interpretExpression (EXP_ASSIGN {lhs, rhs}) =
	UNDEFINED (*TODO*)
;

fun interpretStatement (ST_EXP {exp}) =
	let
		val interpreted = interpretExpression exp;
	in
		TextIO.print ""
	end
|	interpretStatement (ST_BLOCK {stmts=[]}) = TextIO.print ""
|	interpretStatement (ST_BLOCK {stmts}) =
	(interpretStatement (hd stmts); interpretStatement (ST_BLOCK {stmts=(tl stmts)}))
|	interpretStatement (ST_IF {guard, th, el}) =
	let
		val interpreted = interpretExpression guard;
	in
		if getBoolFromValue interpreted
		then interpretStatement th
		else interpretStatement el
	end
|	interpretStatement (ST_PRINT {exp}) =
	let
		val interpreted = interpretExpression exp;
	in
		if valueIsUndefined interpreted
		then TextIO.print "undefined"
		else if valueIsNum interpreted
			 then TextIO.print (Int.toString (getNumFromValue interpreted))
			 else if valueIsBool interpreted
			 	  then if interpreted = BOOLEAN true then TextIO.print "true" else TextIO.print "false"
			 	  else if valueIsId interpreted
			 	  	   then TextIO.print (getIdFromValue interpreted)
			 	  	   else if valueIsString interpreted
			 	  	   		then TextIO.print (getStringFromValue interpreted)
			 	  	   		else errorOut "Unidentified expression in print statement\n"
	end
|	interpretStatement (ST_WHILE {guard, body}) =
	if (interpretExpression guard) = BOOLEAN true
	then (interpretStatement body; interpretStatement (ST_WHILE {guard=guard, body=body}))
	else TextIO.print ""
;

fun interpretStatementList (firstSt::restSt) =
	(interpretStatement firstSt; interpretStatementList restSt);

fun interpretVarList varList =
	true;

fun interpretAST (PROGRAM {decls:string list, stmts:statement list}) =
	(interpretVarList decls; interpretStatementList stmts);

fun interpret fileName =
	let
		val parsedProgram = parse fileName;
		val stateTbl : (string,int) hash_table = mkTable (hash_fn, cmp_fn) (initial_size, CannotFindIt);
	in
		interpretAST parsedProgram
	end;