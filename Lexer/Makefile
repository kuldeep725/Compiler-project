%.lex.sml: %.lex
	mllex $<
	
all :   driver

driver : c.lex.sml driver.mlb driver.sml tokens.sml
		 mlton driver.mlb

run : 
	   ./driver

tests : 
		./driver test1.c test2.c test3.c test4.c test5.c