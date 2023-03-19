%{
#include <stdio.h>
#include <stdlib.h>
#include "listaSimbolos.h"
#include "listaCodigo.h"
#include <string.h>
#include <stdbool.h>
extern int yylex (void);
extern int yylineno;
extern int errores_lexicos;
void yyerror (const char *msj);
int registros[10];
int num_reg = 1;
void inicializar_registros();
int errores_sintacticos = 0;
int errores_semanticos = 0;
void imprimirLS();
void imprimirCodigo(ListaC codigo); 
char* recuperaReg();
void liberaReg(char* reg);
char* concatena(char* c1, char* c2);
void creaInsertaLC(char* op, char* res, char* arg1, char* arg2, ListaC l );
int analisisOk();
bool esConstante(char* n);
void insertaEntrada(char *n, Tipo t);
bool perteneceTablaS(char* n);
char* nuevaEtiqueta();
int contCadenas = 1;
int contador_etiq = 1;


// Lista de simbolos
Lista l;
Tipo t;

%}

// Tipos de datos de los simbolos de la gramatica

%code requires{
    #include "listaCodigo.h"
}

%union{
	char *cadena;
	ListaC codigo;
}



/* Tokens de la gramatica*/

%token	 MAS 			"+"
%token	 MENOS 			"-"
%token	 POR 			"*"
%token	 DIV 			"/"
%token	 PARI 			"("
%token	 PARD 			")"
%token	 LLAVEI 		"{"
%token	 LLAVED 		"}"
%token	 COMA	 		","
/*%token	 <entero>NUMBER	"number"*/
%token	 PYC 			";"
%token	 IGUAL 			"="
%token	 <cadena> ID 	"id"
%token	 <cadena> NUM 	"num"
%token	 <cadena> STR	"string"
%token	 IF			 	"if"
%token	 ELSE			"else"
%token	 WHILE			"while"
%token	 PRINT			"print"
%token	 READ			"read"
%token	 CONST			"const"
%token	 VAR			"var"
%token	 VOID 			"void"
%token	 DO 			"do"
%token	 FOR 			"for"



//Tipos de no terminales

%type <codigo> expression declarations statement statement_list print_item print_list read_list identifier_list asig 

// Asociatividad y precedecia de operadores

%left "+" "-"
%left "*" "/"
%precedence UMINUS


%nonassoc ELSE1
%nonassoc "else"


%start program
%define parse.error verbose


%%

program   	: VOID ID "(" ")" "{" {l = creaLS();} declarations statement_list "}"{

				if(analisisOk()){
					// imprimimos codigo. mips
					//printf("Sale codigo mips\n");
					imprimirLS();
					concatenaLC($7,$8);
					imprimirCodigo($7);

				}
				else{
					printf("----------------------\n");
					printf("Errores lexicos: %d\n", errores_lexicos);
					printf("Errores sintacticos: %d\n", errores_sintacticos);
					printf("Errores semanticos: %d\n", errores_semanticos);
				}
				liberaLC($7);
				liberaLC($8);


			}
			;
		  

declarations	: declarations VAR {t = VARIABLE;}identifier_list ";"{
					$$ = $1;
					concatenaLC($$,$4);
					liberaLC($4);
				}
				| declarations CONST {t = CONSTANTE;} identifier_list ";"{
					$$ = $1;
					concatenaLC($$,$4);
					liberaLC($4);
				}
				|%empty {$$ = creaLC();}
				;
		  		

identifier_list : asig {$$=$1;}
		  		| identifier_list "," asig {$$ = $1;
				  							concatenaLC($$,$3);
											liberaLC($3);}
				;

asig			: ID{
					if (!perteneceTablaS($1)) {
						insertaEntrada($1, t); }
					else{
						printf("Error en la linea %d: identificador %s redefinido\n",yylineno, $1); 
						errores_semanticos++;
					} 
					$$ = creaLC();

					}	
				| ID "=" expression {
					if (!perteneceTablaS($1)) {
						insertaEntrada($1, t);
					}
					else{
						printf("Error en la linea %d: identificador %s redefinido\n",yylineno, $1); 
						errores_semanticos++;
					}
					
					$$=$3;
					if(analisisOk()){
						char * reg = recuperaResLC($3);
						creaInsertaLC("sw", reg, concatena("_", $1), NULL, $$);
						liberaReg(reg);
					}
					

				}
				;
statement_list : statement_list statement{  $$ = $1;
											concatenaLC($$,$2);
											liberaLC($2);
											}
				|%empty {$$ = creaLC();}
				;

statement:		ID "=" expression ";" {
						if (!perteneceTablaS($1)){
							printf("Error en la linea %d: variable %s no declarada\n", yylineno, $1);
							errores_semanticos++;
						}
                        else if (esConstante($1)) {
                            printf("Error en la linea %d: identificador %s es constante\n", yylineno, $1);
							errores_semanticos++;
						}

						$$=$3;
						if(analisisOk()){
							char * reg = recuperaResLC($3);
							creaInsertaLC("sw", reg, concatena("_", $1), NULL, $$);
							liberaReg(reg);
						}
					
				}

				|"{" statement_list "}" {
					$$ = $2;
				}
				|"if" "(" expression ")" statement "else" statement {
					char* reg = recuperaResLC($3);
					char * etiq1 = nuevaEtiqueta();
					char * etiq2 = nuevaEtiqueta();
					$$ = $3;
					creaInsertaLC("beqz",reg,etiq1, NULL, $$);
					liberaReg(reg);
					concatenaLC($$,$5);
					liberaLC($5);
					creaInsertaLC("b",etiq2,NULL, NULL, $$);
					creaInsertaLC("etiq",etiq1,NULL, NULL, $$);
					concatenaLC($$,$7);
					liberaLC($7);
					creaInsertaLC("etiq",etiq2,NULL, NULL, $$);



				}
				|"if" "(" expression ")" statement %prec ELSE1{
					char* reg = recuperaResLC($3);
					char * etiq = nuevaEtiqueta();
					$$ = $3;
					creaInsertaLC("beqz",reg,etiq, NULL, $$);
					liberaReg(reg);
					concatenaLC($$,$5);
					liberaLC($5);
					creaInsertaLC("etiq",etiq,NULL, NULL, $$);
				}
				|"while" "(" expression ")" statement{
					$$ =creaLC();
					char* reg = recuperaResLC($3);
					char * etiq1 = nuevaEtiqueta();
					char * etiq2 = nuevaEtiqueta();
					creaInsertaLC("etiq",etiq1,NULL, NULL, $$);
					concatenaLC($$,$3);
					liberaLC($3);
					creaInsertaLC("beqz",reg,etiq2, NULL, $$);
					liberaReg(reg);
					concatenaLC($$,$5);
					liberaLC($5);
					creaInsertaLC("b",etiq1,NULL, NULL, $$);
					creaInsertaLC("etiq",etiq2,NULL, NULL, $$);
				}
				|"print" print_list ";" {$$ = $2;}
				|"read" read_list ";" {$$ = $2;}
				|"do" statement "while" "(" expression ")" {
					$$ =creaLC();
					char* reg = recuperaResLC($5);
					char * etiq1 = nuevaEtiqueta();
					creaInsertaLC("etiq",etiq1,NULL, NULL, $$);
					concatenaLC($$,$2);
					concatenaLC($$,$5);
					creaInsertaLC("bnez",reg,etiq1, NULL, $$);
					liberaReg(reg);
					liberaLC($2);
					liberaLC($5);
				}
				|"for" "(" expression ")" statement{
					//nuevaEtiqueta
					$$ =creaLC();
					char* reg1 = recuperaResLC($3);
					char* reg2 = recuperaReg();
					char * etiq1 = nuevaEtiqueta();
					char * etiq2 = nuevaEtiqueta();
					//concatenar expresion
					concatenaLC($$,$3);
					liberaLC($3);
					//creaInsertaLC("li", reg1, valor , NULL, $$);
					creaInsertaLC("etiq",etiq1,NULL, NULL, $$);
					
					//comparacion
					creaInsertaLC("blez", reg1, etiq2, NULL, $$);
					creaInsertaLC("beqz", reg1, etiq2, NULL, $$);

					//statement
					concatenaLC($$,$5);
					liberaLC($5);

					//actualizar
					creaInsertaLC("li", reg2, "1", NULL, $$);
					creaInsertaLC("sub", reg1, reg1, reg2, $$);

					//salto a etiqueta 1
					creaInsertaLC("b",etiq1,NULL, NULL, $$);
					creaInsertaLC("etiq",etiq2,NULL, NULL, $$);
					liberaReg(reg2);
					liberaReg(reg1);



				}
				;

print_list: 	 print_item {$$ = $1;}
				| print_list "," print_item {$$ = $1;
											concatenaLC($$,$3);
											liberaLC($3);}
				;

print_item      : expression {
					$$ = $1;
					char * reg = recuperaResLC($1);
					creaInsertaLC("li","$v0","1", NULL, $$);
					creaInsertaLC("move", "$a0", reg, NULL, $$);
					//liberaReg(reg);
					creaInsertaLC("syscall",NULL,NULL, NULL, $$);

				}
				| STR {
					if(!perteneceTablaS($1)){
						Simbolo aux;
						aux.nombre = $1;
						aux.tipo = CADENA;
						aux.valor = contCadenas;
						insertaLS(l, finalLS(l), aux);
						contCadenas++;
					}
					// Llamada al sistema para imprimir por pantalla
					$$ = creaLC();
					char* valor = malloc(sizeof(char));
					PosicionLista p = buscaLS(l, $1);
					Simbolo s =recuperaLS(l,p);
					*valor = s.valor + '0';
					creaInsertaLC("li","$v0","4", NULL, $$);
					creaInsertaLC("la","$a0",concatena("$str", valor), NULL, $$);
					creaInsertaLC("syscall",NULL,NULL, NULL, $$);
					
					}
				;


read_list       : ID  {	if (!perteneceTablaS($1)){
                            printf("Error en la linea %d: variable %s no declarada\n", yylineno, $1);
							errores_semanticos++;
						}
                        else if (esConstante($1)) {
                            printf("Error en la linea %d: identificador %s es constante\n", yylineno, $1);
							errores_semanticos++;
						}
						$$ = creaLC();
							if(analisisOk()){
								creaInsertaLC("li","$v0","5", NULL, $$);
								creaInsertaLC("syscall",NULL,NULL, NULL, $$);
								creaInsertaLC("sw", "$v0", concatena("_", $1), NULL, $$);
							}
					}
						
				| read_list ","  ID  {if (!perteneceTablaS($3)) {
                            printf("Error en la linea %d: variable %s no declarada\n", yylineno, $3);
							errores_semanticos++;
							}
                        else if (esConstante($3)) {
                            printf("Error en la linea %d: identificador %s es constante\n", yylineno, $3);
							errores_semanticos++; }
							
							//concatenar y llamada al sistema para leer
							$$ = creaLC();
							if(analisisOk()){
								creaInsertaLC("li","$v0","5", NULL, $$);
								creaInsertaLC("syscall",NULL,NULL, NULL, $$);
								creaInsertaLC("sw", "$v0", concatena("_", $3), NULL, $$);
							}

							}
				;
				

expression: 	 expression "+" expression {
					$$=$1;
					if(analisisOk()){
						concatenaLC($$,$3);
						char * reg1 = recuperaResLC($1);
						char * reg2 = recuperaResLC($3);
						creaInsertaLC("add", reg1, reg1, reg2, $$);
						liberaLC($3);
						liberaReg(reg2);
					}
				}
				|expression "-" expression {
					$$=$1;
					if(analisisOk()){
						concatenaLC($$,$3);
						char * reg1 = recuperaResLC($1);
						char * reg2 = recuperaResLC($3);
						creaInsertaLC("sub", reg1, reg1, reg2, $$);
						liberaLC($3);
						liberaReg(reg2);
					}
				}
				|expression "*" expression{
					$$=$1;
					if(analisisOk()){
						concatenaLC($$,$3);
						char * reg1 = recuperaResLC($1);
						char * reg2 = recuperaResLC($3);
						creaInsertaLC("mul", reg1, reg1, reg2, $$);
						liberaLC($3);
						liberaReg(reg2);
					}
				}
				|expression "/" expression{
					$$=$1;
					if(analisisOk()){
						concatenaLC($$,$3);
						char * reg1 = recuperaResLC($1);
						char * reg2 = recuperaResLC($3);
						creaInsertaLC("div", reg1, reg1, reg2, $$);
						liberaLC($3);
						liberaReg(reg2);
					}
				}
				|"-" expression %prec UMINUS
				{
					$$=$2;
					if (analisisOk()){
						creaInsertaLC("neg", recuperaResLC($2), recuperaResLC($2), NULL, $$);
						//liberaLC($2);
					}
				}
				|"(" expression ")"
					{$$ = $2;}
				|ID	{
					$$ = creaLC();
					if (!perteneceTablaS($1) ){
                        printf("Error en la linea %d: variable %s no declarada\n", yylineno, $1);
						errores_semanticos++; 
					} 
					
					if (analisisOk()){
						char* reg = recuperaReg();
						creaInsertaLC("lw", reg, concatena("_", $1), NULL, $$);
						guardaResLC($$, reg);
						//liberaReg(reg);

					}
					
				}
				|NUM {
					$$ = creaLC();
					if(analisisOk()){
						char* reg = recuperaReg();
						char* num = $1;
						creaInsertaLC("li", reg, $1, NULL, $$);
						guardaResLC($$, reg);
						//liberaReg(reg);
					}
				}
				;
%%

void yyerror (const char *msj){
       printf("Error en la linea %d : %s\n", yylineno, msj);
}

void inicializar_registros(){
	for(int i = 0; i < 10; i++)
		registros[i] = 0;
}

char* concatena(char* c1, char* c2){
	char* nueva = malloc((strlen(c1)+strlen(c2)+1)*sizeof(char));
	strcpy(nueva, c1);
	strcat(nueva, c2);
	return nueva;
}

char* recuperaReg(){
	for(int i = 0; i< 10; i++)
		if (registros[i] == 0){
			registros[i] = 1;
			char* num = malloc(2*sizeof(char));
			*num = i + '0';
			char* reg = concatena("$t", num);
			return reg;
		}
	return NULL;
}

void liberaReg(char* reg){
	int num = reg[2] - '0';
	registros[num] = 0;
}

int analisisOk(){
	if (errores_semanticos + errores_sintacticos + errores_lexicos == 0) return true;
	return false;
}

void creaInsertaLC(char* op, char* res, char* arg1, char* arg2, ListaC l ){
	Operacion oper;
	oper.op = op;
	oper.res = res;
	oper.arg1 = arg1;
	oper.arg2 = arg2;
	insertaLC(l, finalLC(l), oper);

}

void imprimirLS(){
	PosicionLista p = inicioLS(l);
	printf("############################\n");
	printf(".data\n\n");
	printf("# STRINGS ##################\n");
	//PosicionLista p = inicioLS(l);
	
	while (p != finalLS(l)) {
		Simbolo aux = recuperaLS(l,p);
		if(aux.tipo== CADENA){
			printf("$str%d:		.asciiz %s\n",aux.valor,aux.nombre);
		}
		p = siguienteLS(l,p);
	}
	p = inicioLS(l);
	printf("\n# IDENTIFIERS ##############\n");
	while (p != finalLS(l)) {
		Simbolo aux = recuperaLS(l,p);
		if(aux.tipo != CADENA){
			printf("_%s:	.word 0\n",aux.nombre);
		}
		p = siguienteLS(l,p);
	}
}


// perteneceTablaS() -> buscaLS() Y finalLS()
bool perteneceTablaS(char* n) {
	PosicionLista p = buscaLS(l, n);
	if (p != finalLS(l)) {
		return true;
	}
	return false;
}

// insertaEntrada() -> insertaLS y finalLS
void insertaEntrada(char *n, Tipo t) {
    Simbolo aux;
	aux.nombre = n;
	aux.tipo = t;
	insertaLS(l, finalLS(l), aux);
}

// esConstante() -> buscaLS y recuperaLS
bool esConstante(char* n) {
	PosicionLista p = buscaLS(l, n);
	if(p != finalLS(l)){
		Simbolo s = recuperaLS(l,p);

	if (s.tipo == CONSTANTE) {
		return true;
	}
	return false;
	}	
}

char *nuevaEtiqueta() {
 	char aux[16];
 	sprintf(aux,"$l%d",contador_etiq++);
 	return strdup(aux);
}



void imprimirCodigo(ListaC codigo) {
	printf("###################\n");
	printf("# Seccion de codigo\n");
	printf("	.text\n");
	printf("	.globl main\n");
	printf("main:\n");
	PosicionListaC p = inicioLC(codigo);
	while (p != finalLC(codigo)) {
		Operacion oper = recuperaLC(codigo,p);
		if(strcmp(oper.op,"etiq") == 0){
			printf("%s:",oper.res);
		}
	
		else{
			printf("	%s",oper.op);
			if (oper.res) printf(" %s",oper.res);
		}

		if (oper.arg1) printf(",%s",oper.arg1);
		if (oper.arg2) printf(",%s",oper.arg2);
		printf("\n");
		p = siguienteLC(codigo,p);
	}
	printf("###################\n");
	printf("#FIN\n");
	printf("	jr $ra\n");
	//printf("li $v0, 10\n");
	//printf("syscall\n");

}


/*

int yylex() {
	int c;
	while ((c = getchar ()) == ' ' || c == '\t')
         ;
	
	if (isdigit(c)) {
		yylval=c-'0';
		return DIGITO;
	}
	
	return c;
}

int main (void) {
	
	return yyparse();
  
}*/

