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

fun getIdFromExp (EXP_ID x) = x;

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

fun isValidExpressionEq_Helper stateTbl (EXP_BINARY {opr, lft, rht}) =
	let
		val (leftValue, newStateTbl1) = interpretExpression stateTbl lft;
		val (rightValue, newStateTbl2) = interpretExpression newStateTbl1 rht;
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
(*TODO redo without interpretation of expression*)
and isValidExpression stateTbl (EXP_ASSIGN {lhs, rhs}) =
	true (*TODO*)
|	isValidExpression stateTbl (EXP_UNARY {opr, opnd}) =
	if opr = UOP_NOT
	then (valueIsBool (#1 (interpretExpression stateTbl opnd)))
	else (valueIsNum (#1 (interpretExpression stateTbl opnd)))
|	isValidExpression stateTbl (EXP_BINARY {opr=BOP_EQ, lft, rht}) =
	isValidExpressionEq_Helper stateTbl (EXP_BINARY {opr=BOP_EQ, lft=lft, rht=rht})
|	isValidExpression stateTbl (EXP_BINARY {opr=BOP_NE, lft, rht}) =
	isValidExpressionEq_Helper stateTbl (EXP_BINARY {opr=BOP_NE, lft=lft, rht=rht})
|	isValidExpression stateTbl (EXP_BINARY {opr, lft, rht}) =
	let
		val (leftValue, newStateTbl1) = interpretExpression stateTbl lft;
		val (rightValue, newStateTbl2) = interpretExpression newStateTbl1 rht;
	in
		if binOprIsBool opr andalso valueIsBool leftValue andalso valueIsBool rightValue
		then true
		else if binOprIsMath opr andalso valueIsNum leftValue andalso valueIsNum rightValue
			 then true
			 else if opr = BOP_PLUS andalso valueIsString leftValue andalso valueIsString rightValue
			 	  then true
			 	  else false
	end

and interpretBinaryExpression stateTbl (EXP_BINARY {opr, lft, rht}) =
	if isValidExpression stateTbl (EXP_BINARY {opr=opr, lft=lft, rht=rht})
	then
		if binOprIsBool opr
		then (*Boolean operation*)
			let
				val (interpretedLft, newStateTbl1) = interpretExpression stateTbl lft;
			 	val (interpretedRht, newStateTbl2) = interpretExpression newStateTbl1 rht;
			 	val leftValue = getBoolFromValue interpretedLft;
			 	val rightValue = getBoolFromValue interpretedRht;
			in
				case opr of
					BOP_AND => if not leftValue then (BOOLEAN false, newStateTbl2) else (BOOLEAN rightValue, newStateTbl2)
				|	BOP_OR => if leftValue then (BOOLEAN true, newStateTbl2) else (BOOLEAN rightValue, newStateTbl2)
			end
		else if binOprIsEq opr
			 then (*Eqality operation*)
			 	case opr of
			 		BOP_EQ =>
			 			let
			 				val (interpretedLft, newStateTbl1) = interpretExpression stateTbl lft
			 				val (interpretedRht, newStateTbl2) = interpretExpression newStateTbl1 rht
			 			in
			 				(BOOLEAN (interpretedLft = interpretedRht), newStateTbl2)
			 			end
			 	|	BOP_NE =>
			 			let
			 				val (interpretedLft, newStateTbl1) = interpretExpression stateTbl lft
			 				val (interpretedRht, newStateTbl2) = interpretExpression newStateTbl1 rht
			 			in
			 				(BOOLEAN (not (interpretedLft = interpretedRht)), newStateTbl2)
			 			end
			 else if binOprIsMath opr andalso not (valueIsString (#1 (interpretExpression stateTbl lft)))
			 	  then (*Math operation*)
			 	  	let
			 	  		val (interpretedLft, newStateTbl1) = interpretExpression stateTbl lft;
			 	  		val (interpretedRht, newStateTbl2) = interpretExpression newStateTbl1 rht;
			 	  		val leftValue = getNumFromValue interpretedLft;
			 	  		val rightValue = getNumFromValue interpretedRht;
			 	  	in
			 	  		case opr of
			 	  			BOP_PLUS => (NUMBER (leftValue + rightValue), newStateTbl2)
			 	  		|	BOP_MINUS => (NUMBER (leftValue - rightValue), newStateTbl2)
			 	  		|	BOP_TIMES => (NUMBER (leftValue * rightValue), newStateTbl2)
			 	  		|	BOP_DIVIDE => (NUMBER (leftValue div rightValue), newStateTbl2)
			 	  		|	BOP_MOD => (NUMBER (leftValue mod rightValue), newStateTbl2)
			 	  		|	BOP_LT => (BOOLEAN (leftValue < rightValue), newStateTbl2)
			 	  		|	BOP_LE => (BOOLEAN (leftValue <= rightValue), newStateTbl2)
			 	  		|	BOP_GT => (BOOLEAN (leftValue > rightValue), newStateTbl2)
			 	  		|	BOP_GE => (BOOLEAN (leftValue >= rightValue), newStateTbl2)
			 	  	end
			 	  else if opr = BOP_PLUS
			 	  	   then 
			 	  	   		let
			 	  	   			val (interpretedLft, newStateTbl1) = interpretExpression stateTbl lft;
			 	  	   			val (interpretedRht, newStateTbl2) = interpretExpression newStateTbl1 rht;
			 	  	   		in
			 	  	   			(STRING (getStringFromValue interpretedLft ^ getStringFromValue interpretedRht), newStateTbl2)
			 	  	   		end
			 	  	   else errorOut "Error in binary operation\n"
	else errorOut "Bad type in binary expression\n"

and interpretExpression stateTbl EXP_TRUE = (BOOLEAN true, stateTbl)
|	interpretExpression stateTbl EXP_FALSE = (BOOLEAN false, stateTbl)
|	interpretExpression stateTbl (EXP_STRING x) = (STRING x, stateTbl)
|	interpretExpression stateTbl (EXP_NUM x) = (NUMBER x, stateTbl)
|	interpretExpression stateTbl EXP_UNDEFINED = (UNDEFINED, stateTbl)
|	interpretExpression stateTbl (EXP_ID x) = 
	let
		val optionValue = find stateTbl x;
	in
		if isSome optionValue
		then (NUMBER (valOf optionValue), stateTbl)
		else errorOut ("Variable not found: '" ^ x ^ "'\n")
	end
|	interpretExpression stateTbl (EXP_BINARY {opr, lft, rht}) = 
	interpretBinaryExpression stateTbl (EXP_BINARY {opr=opr, lft=lft, rht=rht})
|	interpretExpression stateTbl (EXP_UNARY {opr, opnd}) =
	if isValidExpression stateTbl (EXP_UNARY {opr=opr, opnd=opnd})
	then
		case opr of
			UOP_NOT =>
				let
					val (interpreted, newStateTbl) = interpretExpression stateTbl opnd;
				in
					(BOOLEAN (not (getBoolFromValue interpreted)), newStateTbl)
				end
		|	UOP_MINUS =>
				let
					val (interpreted, newStateTbl) = interpretExpression stateTbl opnd;
				in
					(NUMBER (~ (getNumFromValue interpreted)), newStateTbl)
				end
	else errorOut "Bad type in unary expression\n"
|	interpretExpression stateTbl (EXP_COND {guard, thenExp, elseExp}) =
	let
		val (interpreted, newStateTbl) = interpretExpression stateTbl guard;
	in
		if interpreted = BOOLEAN true
		then interpretExpression newStateTbl thenExp
		else if interpreted = BOOLEAN false
			 then interpretExpression newStateTbl elseExp
			 else errorOut "Guard of conditional expression must be of boolean type\n"
	end
|	interpretExpression stateTbl (EXP_ASSIGN {lhs, rhs}) =
	let
		val (interpreted, newStateTbl) = interpretExpression stateTbl rhs;
		val assignedId = getIdFromExp lhs;
		val testIfDeclared = find stateTbl assignedId;
	in
		if not (isSome testIfDeclared)
		then errorOut ("Variable '" ^ assignedId ^ "' not declared\n")
		else (insert newStateTbl (assignedId, getNumFromValue interpreted); (ID (getIdFromExp lhs), stateTbl))
	end
;

fun interpretStatement stateTbl (ST_EXP {exp}) =
	let
		val (interpreted, newStateTbl) = interpretExpression stateTbl exp;
	in
		newStateTbl
	end
|	interpretStatement stateTbl (ST_BLOCK {stmts=[]}) = stateTbl
|	interpretStatement stateTbl (ST_BLOCK {stmts}) =
	let
		val newStateTbl = interpretStatement stateTbl (hd stmts);
	in
		interpretStatement newStateTbl (ST_BLOCK {stmts=(tl stmts)})
	end
|	interpretStatement stateTbl (ST_IF {guard, th, el}) =
	let
		val (interpreted, newStateTbl) = interpretExpression stateTbl guard;
	in
		if getBoolFromValue interpreted
		then interpretStatement newStateTbl th
		else interpretStatement newStateTbl el
	end
|	interpretStatement stateTbl (ST_PRINT {exp}) =
	let
		val (interpreted, newStateTbl) = interpretExpression stateTbl exp;
	in
		if valueIsUndefined interpreted
		then (TextIO.print "undefined"; newStateTbl)
		else if valueIsNum interpreted
			 then (TextIO.print (Int.toString (getNumFromValue interpreted)); newStateTbl)
			 else if valueIsBool interpreted
			 	  then if interpreted = BOOLEAN true then (TextIO.print "true"; newStateTbl) else (TextIO.print "false"; newStateTbl)
			 	  else if valueIsId interpreted
			 	  	   then (TextIO.print (getIdFromValue interpreted); newStateTbl)
			 	  	   else if valueIsString interpreted
			 	  	   		then (TextIO.print (getStringFromValue interpreted); newStateTbl)
			 	  	   		else errorOut "Unidentified expression in print statement\n"
	end
|	interpretStatement stateTbl (ST_WHILE {guard, body}) =
	let
		val (interpreted, newStateTbl) = interpretExpression stateTbl guard
	in
		if interpreted = BOOLEAN true
		then
			let
				val newStateTbl2 = interpretStatement newStateTbl body;
			in
				interpretStatement newStateTbl2 (ST_WHILE {guard=guard, body=body})
			end
		else newStateTbl
	end
;

fun interpretStatementList stateTbl [] = stateTbl
|	interpretStatementList stateTbl (firstSt::restSt) =
	let
		val newStateTbl = interpretStatement stateTbl firstSt;
	in
		interpretStatementList newStateTbl restSt
	end
;

fun interpretVarList stateTbl [] = stateTbl
|	interpretVarList stateTbl (firstString::restStrings) =
	if not (isSome (find stateTbl firstString))
	then
		let
			val nothing = insert stateTbl (firstString, ~9999); (*Ask what value to initialize*)
		in
			interpretVarList stateTbl restStrings
		end
	else errorOut ("Variable already declared: '" ^ firstString ^ "'\n")
;

fun interpretAST stateTbl (PROGRAM {decls:string list, stmts:statement list}) =
	let
		val newStateTbl = interpretVarList stateTbl decls;
	in
		interpretStatementList newStateTbl stmts
	end
;

fun interpret fileName =
	let
		val parsedProgram = parse fileName;
		val stateTbl : (string,int) hash_table = mkTable (hash_fn, cmp_fn) (initial_size, CannotFindIt);
	in
		interpretAST stateTbl parsedProgram
	end;