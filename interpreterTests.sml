use "interpreter.sml";

open HashTable;

exception CannotFindIt;

val hash_fn : string->word = HashString.hashString;
val cmp_fn : string*string->bool = (op =);
val initial_size : int = 101;

val stateTbl : (string,interpretValue) hash_table = mkTable (hash_fn, cmp_fn) (initial_size, CannotFindIt);

#1 (interpretExpression stateTbl EXP_TRUE) = BOOLEAN true;
#1 (interpretExpression stateTbl EXP_FALSE) = BOOLEAN false;
#1 (interpretExpression stateTbl (EXP_STRING "abc")) = STRING "abc";
#1 (interpretExpression stateTbl (EXP_NUM 5)) = NUMBER 5;
#1 (interpretExpression stateTbl EXP_UNDEFINED) = UNDEFINED;

#1 (interpretExpression stateTbl (EXP_BINARY {opr=BOP_PLUS, lft=(EXP_NUM 2), rht=(EXP_NUM 4)})) = NUMBER 6;
#1 (interpretExpression stateTbl (EXP_BINARY {opr=BOP_PLUS, lft=(EXP_STRING "a"), rht=(EXP_STRING "bc")})) = STRING "abc";
#1 (interpretExpression stateTbl (EXP_BINARY {opr=BOP_MINUS, lft=(EXP_NUM 2), rht=(EXP_NUM 5)})) = NUMBER ~3;
#1 (interpretExpression stateTbl (EXP_BINARY {opr=BOP_MOD, lft=(EXP_NUM 5), rht=(EXP_NUM 2)})) = NUMBER 1;
#1 (interpretExpression stateTbl (EXP_BINARY {opr=BOP_GE, lft=(EXP_NUM 10), rht=(EXP_NUM 7)})) = BOOLEAN true;
#1 (interpretExpression stateTbl (EXP_BINARY {opr=BOP_DIVIDE, lft=(EXP_NUM 15), rht=(EXP_NUM 5)})) = NUMBER 3;
#1 (interpretExpression stateTbl (EXP_BINARY {opr=BOP_TIMES, lft=EXP_BINARY {opr=BOP_PLUS, lft=(EXP_NUM 2), rht=(EXP_NUM 3)}, rht=(EXP_NUM 2)})) = NUMBER 10;

#1 (interpretExpression stateTbl (EXP_UNARY {opr=UOP_NOT, opnd=EXP_TRUE})) = BOOLEAN false;
#1 (interpretExpression stateTbl (EXP_UNARY {opr=UOP_MINUS, opnd=(EXP_NUM 5)})) = NUMBER ~5;

#1 (interpretExpression stateTbl (EXP_COND {guard=EXP_TRUE, thenExp=(EXP_NUM 5), elseExp=(EXP_STRING "abc")})) = NUMBER 5;
#1 (interpretExpression stateTbl (EXP_COND {guard=EXP_FALSE, thenExp=(EXP_NUM 5), elseExp=(EXP_STRING "abc")})) = STRING "abc";

interpretVarList stateTbl ["testVar"];

#1 (interpretExpression stateTbl (EXP_ASSIGN {lhs=(EXP_ID "testVar"), rhs=(EXP_NUM 5)})) = NUMBER 5;

#1 (interpretExpression stateTbl (EXP_ID "testVar")) = NUMBER 5;