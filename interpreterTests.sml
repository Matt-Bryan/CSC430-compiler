use "interpreter.sml";

interpretExpression EXP_TRUE = BOOLEAN true;
interpretExpression EXP_FALSE = BOOLEAN false;
interpretExpression (EXP_STRING "abc") = STRING "abc";
interpretExpression (EXP_NUM 5) = NUMBER 5;
interpretExpression EXP_UNDEFINED = UNDEFINED;

interpretExpression (EXP_BINARY {opr=BOP_PLUS, lft=(EXP_NUM 2), rht=(EXP_NUM 4)}) = NUMBER 6;
interpretExpression (EXP_BINARY {opr=BOP_PLUS, lft=(EXP_STRING "a"), rht=(EXP_STRING "bc")}) = STRING "abc";
interpretExpression (EXP_BINARY {opr=BOP_MINUS, lft=(EXP_NUM 2), rht=(EXP_NUM 5)}) = NUMBER ~3;
interpretExpression (EXP_BINARY {opr=BOP_MOD, lft=(EXP_NUM 5), rht=(EXP_NUM 2)}) = NUMBER 1;
interpretExpression (EXP_BINARY {opr=BOP_GE, lft=(EXP_NUM 10), rht=(EXP_NUM 7)}) = BOOLEAN true;
interpretExpression (EXP_BINARY {opr=BOP_DIVIDE, lft=(EXP_NUM 15), rht=(EXP_NUM 5)}) = NUMBER 3;
interpretExpression (EXP_BINARY {opr=BOP_TIMES, lft=EXP_BINARY {opr=BOP_PLUS, lft=(EXP_NUM 2), rht=(EXP_NUM 3)}, rht=(EXP_NUM 2)}) = NUMBER 10;

interpretExpression (EXP_UNARY {opr=UOP_NOT, opnd=EXP_TRUE}) = BOOLEAN false;
interpretExpression (EXP_UNARY {opr=UOP_MINUS, opnd=(EXP_NUM 5)}) = NUMBER ~5;

interpretExpression (EXP_COND {guard=EXP_TRUE, thenExp=(EXP_NUM 5), elseExp=(EXP_STRING "abc")}) = NUMBER 5;
interpretExpression (EXP_COND {guard=EXP_FALSE, thenExp=(EXP_NUM 5), elseExp=(EXP_STRING "abc")}) = STRING "abc";

(*interpretExpression (EXP_BINARY {opr, lft, rht}) = ; *)