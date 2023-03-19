#include <stdio.h>
#include <stdlib.h>
extern FILE *yyin;
extern int yyparse();


int main(int argc, char const *argv[])
{
    if(argc != 2){
        printf("Uso: %s prueba.txt\n", argv[0]);
        exit(1);
    }

    yyin = fopen(argv[1], "r");
    if(yyin == NULL){
        printf("Error abriendo %s\n", argv[1]);
        exit(2);
    }
    // Analizar sintacticamente
    return yyparse();
}
