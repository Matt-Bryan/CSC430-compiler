use "parser.sml";

datatype interpretValue =
	BOOLEAN of bool
|	STRING of string
|	NUMBER of int
|	ID of string
|	UNDEFINED
;

open HashTable;

exception CannotFindIt;

val hash_fn : string->word = HashString.hashString;
val cmp_fn : string*string->bool = (op =);
val initial_size : int = 101;

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

fun interpretBinaryExpression stateTbl (EXP_BINARY {opr, lft, rht}) =
	if binOprIsBool opr
	then (*Boolean operation*)
		let
			val (interpretedLft, newStateTbl1) = interpretExpression stateTbl lft;
			val leftValue = if valueIsBool interpretedLft then getBoolFromValue interpretedLft else errorOut "And/or must have boolean operands\n";
		in
			if opr = BOP_OR
			then if leftValue
				 then (BOOLEAN true, newStateTbl1)
				 else
				 	let
				 		val (interpretedRht, newStateTbl2) = interpretExpression newStateTbl1 rht;
						val rightValue = getBoolFromValue interpretedRht;
				 	in
				 		(BOOLEAN rightValue, newStateTbl2)
				 	end
			else
				if not leftValue then (BOOLEAN false, newStateTbl1) else
				let
				 	val (interpretedRht, newStateTbl2) = interpretExpression newStateTbl1 rht;
					val rightValue = getBoolFromValue interpretedRht;
				in
				 	(BOOLEAN (leftValue andalso rightValue), newStateTbl2)
				end
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
		 else (*Math operation*)
			 	let
			 	  	val (interpretedLft, newStateTbl1) = interpretExpression stateTbl lft;
			 	  	val (interpretedRht, newStateTbl2) = interpretExpression newStateTbl1 rht;
			 	  	val leftValue = if valueIsString interpretedLft
			 	  					then if not (valueIsString interpretedRht)
										 then errorOut "Types must match in arithmetic operator\n"
										 else 0
									else if not (valueIsNum interpretedRht)
										 then errorOut "Types must match in arithmetic operator\n"
										 else getNumFromValue interpretedLft;
			 	  	val rightValue = if valueIsString interpretedRht
			 	  					 then 0
			 	  					 else getNumFromValue interpretedRht;
			 	in
			 		if not (opr = BOP_PLUS) andalso valueIsString interpretedLft then errorOut "Int type required for arithmetic operations\n" else
			 	  	case opr of
			 	  		BOP_PLUS => if valueIsNum interpretedLft
			 	  					then (NUMBER (leftValue + rightValue), newStateTbl2)
			 	  					else (STRING (getStringFromValue interpretedLft ^ getStringFromValue interpretedRht), newStateTbl2)
			 	  	|	BOP_MINUS => (NUMBER (leftValue - rightValue), newStateTbl2)
			 	  	|	BOP_TIMES => (NUMBER (leftValue * rightValue), newStateTbl2)
			 	  	|	BOP_DIVIDE => (NUMBER (leftValue div rightValue), newStateTbl2)
			 	  	|	BOP_MOD => (NUMBER (leftValue mod rightValue), newStateTbl2)
			 	  	|	BOP_LT => (BOOLEAN (leftValue < rightValue), newStateTbl2)
			 	  	|	BOP_LE => (BOOLEAN (leftValue <= rightValue), newStateTbl2)
			 	  	|	BOP_GT => (BOOLEAN (leftValue > rightValue), newStateTbl2)
			 	  	|	BOP_GE => (BOOLEAN (leftValue >= rightValue), newStateTbl2)
			 	end
			 	(*else if opr = BOP_PLUS
			 				 	  	 then 
			 				 	  	   	let
			 				 	  	   		val (interpretedLft, newStateTbl1) = interpretExpression stateTbl lft;
			 				 	  	   		val (interpretedRht, newStateTbl2) = interpretExpression newStateTbl1 rht;
			 				 	  	   	in
			 				 	  	   		(STRING (getStringFromValue interpretedLft ^ getStringFromValue interpretedRht), newStateTbl2)
			 				 	  	   	end
			 				 	  	else errorOut "Error in binary operation\n"*)

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
		then (valOf optionValue, stateTbl)
		else errorOut ("Variable not found: '" ^ x ^ "'\n")
	end
|	interpretExpression stateTbl (EXP_BINARY {opr, lft, rht}) = 
	interpretBinaryExpression stateTbl (EXP_BINARY {opr=opr, lft=lft, rht=rht})
|	interpretExpression stateTbl (EXP_UNARY {opr, opnd}) =
	if opr = UOP_NOT
	then
		let
			val (interpreted, newStateTbl) = interpretExpression stateTbl opnd;
		in
			if not (valueIsBool interpreted) then errorOut "Not requires boolean type\n" else
			(BOOLEAN (not (getBoolFromValue interpreted)), newStateTbl)
		end
	else
		let
			val (interpreted, newStateTbl) = interpretExpression stateTbl opnd;
		in
			if not (valueIsNum interpreted) then errorOut "Minus requires number type\n" else
			(NUMBER (~ (getNumFromValue interpreted)), newStateTbl)
		end
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
		else (insert newStateTbl (assignedId, interpreted); (interpretExpression newStateTbl lhs))
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
			 then
			 	let
			 		val number = getNumFromValue interpreted;
			 	in
			 		if number < 0
			 		then (TextIO.print ("-" ^ (Int.toString (number * ~1))); newStateTbl)
			 		else (TextIO.print (Int.toString number); newStateTbl)
			 	end
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
			val nothing = insert stateTbl (firstString, UNDEFINED); (*Ask what value to initialize*)
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
		val stateTbl : (string, interpretValue) hash_table = mkTable (hash_fn, cmp_fn) (initial_size, CannotFindIt);
	in
		interpretAST stateTbl parsedProgram
	end;