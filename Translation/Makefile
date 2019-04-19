%.lex.sml: %.lex
	mllex $<

%.grm.sml: %.grm
	mlyacc $<
	
all :   driver

driver : c.lex.sml driver.mlb driver.sml c.grm.sml ast.sml semant.sml env.sml symbol.sml table.sig table.sml translate.sml types.sml java_ast.sml 
		 mlton driver.mlb

run : 
	   ./driver