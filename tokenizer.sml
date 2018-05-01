use "tokens.sml";

exception InvalidSymbol of string;
exception InvalidEscapeSequence of char;
exception InvalidEscapeMissing;
exception UnterminatedString;

Control.Print.printDepth := 20;
Control.Print.printLength := 100;

fun identify curString = 
	case curString of
	   "anon" => TK_ANON
	 | "class" => TK_CLASS
	 | "else" => TK_ELSE
	 | "extends" => TK_EXTENDS
	 | "false" => TK_FALSE
	 | "fun" => TK_FUN
	 | "if" => TK_IF
	 | "name" => TK_NAME
	 | "new" => TK_NEW
	 | "print" => TK_PRINT
	 | "ref" => TK_REF
	 | "return" => TK_RETURN
	 | "super" => TK_SUPER
	 | "this" => TK_THIS
	 | "true" => TK_TRUE
	 | "undefined" => TK_UNDEFINED
	 | "var" => TK_VAR
	 | "while" => TK_WHILE
	 | "=" => TK_ASSIGN
	 | "{" => TK_LBRACE
	 | "}" => TK_RBRACE
	 | "(" => TK_LPAREN
	 | ")" => TK_RPAREN
	 | "," => TK_COMMA
	 | ";" => TK_SEMI
	 | "?" => TK_QUESTION
	 | ":" => TK_COLON
	 | "." => TK_DOT
	 | "&&" => TK_AND
	 | "||" => TK_OR
	 | "==" => TK_EQ
	 | "!=" => TK_NE
	 | "<" => TK_LT
	 | ">" => TK_GT
	 | "<=" => TK_LE
	 | ">=" => TK_GE
	 | "+" => TK_PLUS
	 | "-" => TK_MINUS
	 | "*" => TK_TIMES
	 | "/" => TK_DIVIDE
	 | "%" => TK_MOD
	 | "!" => TK_NOT
	 | _ => if (Char.isAlpha (String.sub (curString, 0)))
	 		then TK_ID curString
	 		else if (Char.isDigit (String.sub (curString, 0)))
	 			 then TK_NUM (valOf (Int.fromString curString))
	 			 else raise InvalidSymbol curString;

fun isTrickyChar thisChar =
	if thisChar = #"&" orelse thisChar = #"|" orelse thisChar = #"=" orelse thisChar = #"!" orelse thisChar = #"<" orelse thisChar = #">"
	then true
	else false;

fun isNonAlphaNum thisChar =
	if Char.isAlpha thisChar orelse Char.isDigit thisChar
	then false
	else true;

fun isNextCharValid firstOfCurString nextChar = 
	if (Char.isAlpha firstOfCurString)
	then if (Char.isAlpha nextChar orelse Char.isDigit nextChar)
		 then true
		 else false
	else if (Char.isDigit firstOfCurString)
		 then if (Char.isDigit nextChar)
		 	  then true
		 	  else false
		 else if (Char.isDigit nextChar orelse Char.isAlpha nextChar)
		 	  then false
		 	  else true;

fun notValidEscape theChar =
	if theChar <> #"\"" andalso theChar <> #"\\" andalso theChar <> #"\n" andalso theChar <> #"\t"
	then true
	else false;

fun createAndAddToken explodedList tokenList curString = 
	let
		val newToken = identify curString;
		val newTokenList = tokenList @ [newToken];
	in
		if null explodedList
		then newTokenList
		else parseExplodedList (tl explodedList) newTokenList ""
	end

and parseExplodedList [] [] "" = []
|	parseExplodedList [] tokenList "" = tokenList
|	parseExplodedList explodedList tokenList curString = 
	if null explodedList
	then if (String.size curString) = 0
		 then tokenList
		 else createAndAddToken [] tokenList curString
	else if (Char.isSpace (hd explodedList))
		 then if (String.size curString) = 0
		 	  then parseExplodedList (tl explodedList) tokenList ""
		 	  else createAndAddToken explodedList tokenList curString
		 else if ((hd explodedList) = #"\"")
		 	  then stringHandler (tl explodedList) tokenList curString
		 	  else if isNonAlphaNum (hd explodedList)
		 	  	   then nonAlphaNumHandler explodedList tokenList curString
		 	  	   else if (String.size curString) = 0
				 	    then parseExplodedList (tl explodedList) tokenList (curString ^ (str (hd explodedList)))
				 	    else if isNextCharValid (String.sub (curString, 0)) (hd explodedList)
				 	         then parseExplodedList (tl explodedList) tokenList (curString ^ (str (hd explodedList)))
				 	         else createAndAddToken (#"0"::explodedList) tokenList curString

and stringHandler [] tokenList curString =
	raise UnterminatedString
|	stringHandler explodedList tokenList curString =
	if (hd explodedList) = #"\\"
	then if List.length explodedList < 2
		 then raise InvalidEscapeMissing
		 else if (hd (tl explodedList)) <> #"n" andalso (hd (tl explodedList)) <> #"t" andalso (hd (tl explodedList)) <> #"\"" andalso (hd (tl explodedList)) <> #"\\"
		 	  then raise InvalidEscapeSequence (hd (tl explodedList))
		 	  else stringHandler (tl (tl explodedList)) tokenList (curString ^ str (valOf (Char.fromString ("\\" ^ (str (hd (tl explodedList)))) ) )   )
	else if (hd explodedList) = #"\""
		 then
			let
				val newToken = TK_STRING curString;
				val newTokenList = tokenList @ [newToken];
			in
				if null explodedList
				then newTokenList
				else parseExplodedList (tl explodedList) newTokenList ""
			end
		 else stringHandler (tl explodedList) tokenList (curString ^ (str (hd explodedList)))

and nonAlphaNumHandler explodedList tokenList curString =
	if (String.size curString) <> 0
	then let
			val newToken = identify curString;
			val newTokenList = tokenList @ [newToken];
		 in
			nonAlphaNumHandler explodedList newTokenList ""
		 end
	else let
			val firstChar = (hd explodedList);
			val secondChar = if (List.length explodedList < 2)
							 then if (firstChar = #"&" orelse firstChar = #"|")
							 	  then raise InvalidSymbol (str firstChar)
							 	  else #"^"
							 else (hd (tl explodedList));
		 in
			if isTrickyChar firstChar
			then case firstChar of
					#"&" => if secondChar = #"&"
							then createAndAddToken (tl explodedList) tokenList ((str firstChar) ^ (str secondChar))
							else raise InvalidSymbol (str firstChar)
				|	#"|" => if secondChar = #"|"
							then createAndAddToken (tl explodedList) tokenList ((str firstChar) ^ (str secondChar))
							else raise InvalidSymbol (str firstChar)
				|	#"=" => if secondChar = #"="
							then createAndAddToken (tl explodedList) tokenList ((str firstChar) ^ (str secondChar))
							else createAndAddToken explodedList tokenList (str firstChar)
				|	#"!" => if secondChar = #"="
							then createAndAddToken (tl explodedList) tokenList ((str firstChar) ^ (str secondChar))
							else createAndAddToken explodedList tokenList (str firstChar)
				|	#"<" => if secondChar = #"="
							then createAndAddToken (tl explodedList) tokenList ((str firstChar) ^ (str secondChar))
							else createAndAddToken explodedList tokenList (str firstChar)
				|	#">" => if secondChar = #"="
							then createAndAddToken (tl explodedList) tokenList ((str firstChar) ^ (str secondChar))
							else createAndAddToken explodedList tokenList (str firstChar)
			else createAndAddToken explodedList tokenList (str firstChar)
		 end;

fun tokenize "" = []
|	tokenize inputString = 
	let
		val explodedList = explode inputString;
	in
		parseExplodedList explodedList [] ""
	end;