TOP=..
include ${TOP}/Makefile.defs

OPT = -O2

expr : Expr.hs Main.lhs 
	# might want to run happy with --ghc
	${GHC} -cpp -fglasgow-exts -package lang -o expr --make Main.lhs
                                                                                
run : expr
	./expr "1+2*4-3"

runn : expr
	./expr +RTS -s -RTS `perl -e 'print join ("+", (1 .. ${NUM}));'` | tee out-${NUM}
	cat expr.stat >> out-${NUM}


eof : expr
	echo testing premature eof
	./expr "1+2*"

err : expr
	echo testing syntax error 
	./expr "1+2*2++3"

test : run eof err

clean :
	rm -rf expr Expr.info Expr.hs ExprData.hs *.o *.hi out.daVinci 
