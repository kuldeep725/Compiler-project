%.lex.sml: %.lex
	mllex $<

%.grm.sml: %.grm
	mlyacc $<
	
all :   driver

driver : c.lex.sml driver.mlb driver.sml c.grm.sml ast.sml 
		 mlton driver.mlb

run : 
	   ./driver