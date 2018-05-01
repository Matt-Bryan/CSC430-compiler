datatype token =
     TK_LBRACE
   | TK_RBRACE
   | TK_LPAREN
   | TK_RPAREN
   | TK_COMMA
   | TK_SEMI
   | TK_QUESTION
   | TK_COLON
   | TK_DOT
   | TK_PLUS
   | TK_MINUS
   | TK_TIMES
   | TK_DIVIDE
   | TK_MOD
   | TK_AND
   | TK_OR
   | TK_ASSIGN
   | TK_EQ
   | TK_LT
   | TK_LE
   | TK_GT
   | TK_GE
   | TK_NOT
   | TK_NE
   | TK_ANON
   | TK_CLASS
   | TK_ELSE
   | TK_EXTENDS
   | TK_FALSE
   | TK_FUN
   | TK_IF
   | TK_NAME
   | TK_NEW
   | TK_PRINT
   | TK_REF
   | TK_RETURN
   | TK_SUPER
   | TK_THIS
   | TK_TRUE
   | TK_UNDEFINED
   | TK_VAR
   | TK_WHILE
   | TK_NUM of int
   | TK_ID of string
   | TK_STRING of string
;