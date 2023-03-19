/* Códugo literal y macros de ER */

%{
    #include <stdio.h>
    #include "miniC.tab.h" 
    #include <string.h>
    void error_lexico(); 
    int errores_lexicos = 0; 
    int inicio_comentario;
%}

digito [0-9]
letra [a-zA-Z_]
panico [^ \n\r\ta-zA-Z_0-9+\-*/();=,{}]

%option yylineno

%x      comentario 


%%

 /* Expresiones regulares de comentarios y blanco */
[ \t\r\n]+                      { }
"//".*                          { }
"/*"                            {inicio_comentario = yylineno ;BEGIN(comentario); }  
<comentario>"*/"                {BEGIN(INITIAL);    }
<comentario>(.|\n)              { }


<comentario><<EOF>>        { printf("ERROR: comentario no cerrado en línea %d\n", inicio_comentario);
                             errores_lexicos++; 
                             return 0;}

 /* Expresiones regulares de tokens*/
"void"                          { return VOID; }
"var"                           { return VAR; }
"const"                         { return CONST; }
"if"                            { return IF; }
"else"                          { return ELSE; }
"while"                         { return WHILE; }
"print"                         { return PRINT; }
"read"                          { return READ; }
"do"                            { return DO;}
"for"                            { return FOR;}

{letra}({letra}|{digito})*      { if (yyleng > 16) {
                                        printf("ERROR: identificador demasiado largo en linea %d: %s\n",yylineno, yytext);
                                        errores_lexicos++;
                                        } 
                                  else 
                                      yylval.cadena = strdup(yytext);
                                  return ID; }
"+"                             { return MAS; }
"-"                             { return MENOS; }
"*"                             { return POR; }
"/"                             { return DIV; }
"("                             { return PARI; }
")"                             { return PARD; }
"{"                             { return LLAVEI; }
"}"                             { return LLAVED; }
";"                             { return PYC; }
","                             { return COMA; }
"="                             { return IGUAL; }
{digito}+                       {   yylval.cadena = strdup(yytext);
                                    if (atol(yytext) > 2147483648 || atol(yytext) < -2147483648) 
                                        {printf("ERROR: entero demasiado grande en linea %d: %s\n", yylineno, yytext);
                                        errores_lexicos++; } 
                                    return NUM; } 
\"([^"\n]|\\\")*\"              { yylval.cadena = strdup(yytext); return STR; }
\"([^"\n]|\\\")*                { printf("ERROR: cadena sin cerrar en linea %d\n", yylineno); 
                                        errores_lexicos++;}
{panico}+                       { printf("ERROR: caracteres no validos en linea %d: %s\n", yylineno, yytext);
                                        errores_lexicos++;}




%%

/*int main(int argc, char *argv[]){

    int token;
    while ((token = yylex()) != 0) {
        printf("Token %d: %s\n", token, yytext);
    }

}*/

void error_lexico() {
    printf("Error lexico en linea %d: %s\n", yylineno, yytext);
}
