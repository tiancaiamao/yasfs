
OBJ analyze_define(OBJ sexp,OBJ env)
{
	建一个环境cell加到env中;
	产生一个define结构的ast;
	struct ast
	{
		OBJ value;
		OBJ cell;
	};

}
OBJ analyze_if(){}
OBJ analyze_set(){}
OBJ analyze_lambda(OBJ sexp,OBJ env)
{
	新建env，其父env是传入的env;
	将参数全binding到env，value为undefined;
	生成lambda结构的ast;
	struct ast
	{
		OBJ env = 新的env;
		OBJ para = 取出para构成一个list;
		OBJ body = body部分;
	};	
}
OBJ analyze_if(OBJ sexp,OBJ env)
{
	生成if结构的ast;
	struct ast
	{
		test_part;
		then_part;
		else_part;
	};
}
OBJ analyze(OBJ sexp,OBJ env)
{
}

OBJ generate_define()
{
	generate（value）;
	emit(PUSH);
	emit(cell);
	emit(set-cdr);
}
OBJ generate_if(OBJ ast,OBJ env)
{
	generate(sexp->test_part);
	emit(JUMP_UNLESS);
	emit(label1);
	generate(sexp->then_part);
	emit(JUMP);
	emit(label2);
	make_label(label1);
	generate(sexp->else_part);
	make_label(label2);
}
generate_if()
generate_set()
generate_lambda()
OBJ generate_lambda(OBJ ast,OBJ env)
{
	generate(ast->body,ast->env);
	生成RET;
}
OBJ generate(OBJ ast,OBJ env)
{
}

int compile(OBJ sexp,OBJ env)
{
}
