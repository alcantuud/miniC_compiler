 # Instrucciones de compilación de léxico

lexico : lex.yy.c
	gcc main.c lex.yy.c -lfl -o lexico

lex.yy.c : lexico.l lexico.h
	flex lexico.l

clean :
	rm -f lexico lex.yy.c

run : lexico prueba.txt
	./lexico < prueba.txt