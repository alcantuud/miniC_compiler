all: miniC lexico

miniC :		main.c lex.yy.c miniC.tab.c listaSimbolos.c listaCodigo.c
		  gcc -g main.c lex.yy.c miniC.tab.c listaSimbolos.c listaCodigo.c -lfl -o miniC

lexico :main_lexico.c lex.yy.c miniC.tab.c listaSimbolos.c listaCodigo.c
		  gcc -g main_lexico.c lex.yy.c miniC.tab.c listaSimbolos.c listaCodigo.c -lfl -o lexico

lex.yy.c : miniC.l miniC.tab.h
	flex miniC.l
	
miniC.tab.c miniC.tab.h : miniC.y
	bison -d -v miniC.y

clean : 
	rm -f miniC.tab.* lex.yy.c miniC lexico

lex: lexico test_lex.mc.txt
	./lexico test_lex.mc.txt > salida_lexico.txt

sintactico: miniC test_lex.mc.txt
	./miniC prueba_sintactico.mc > salida_sintactico.txt

run: miniC imput.txt
	./miniC imput.txt > output.s
