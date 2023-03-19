#include <stdio.h>
#include <stdlib.h>
extern int yylex();
extern int errores_lexicos;
extern char *yytext;
extern FILE *yyin;


int main(int argc, char *argv[]){

    if(argc != 2){
        printf("Uso: %s fichero.txt\n", argv[0]);
        exit(1);
    }

    yyin = fopen(argv[1], "r");
    if(yyin == NULL){   
        printf("No se puede abrir %s\n", argv[1]);
        exit(2);
    }
    
    int token;

    while ((token = yylex()) != 0) {
        //printf("Token %d: %s\n", token, yytext);
    }

    printf("----------------------\n");
    printf("Errores lexicos: %d\n",errores_lexicos );

}