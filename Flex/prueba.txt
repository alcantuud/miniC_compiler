/**************************************
 * Fichero de prueba de análisis léxico
 **************************************/

void test2 () {

// Entero demasiado grande
const a = 123456789012;

// Identificador demasiado largo
_123456789012345678 = 1;

// Secuencia de caracteres incorrectos
print #@$%&;

// Cadena de caracteres sin cerrar
print "Hola\";
a = 1;

// Comentario sin cerrar
/* Esto no va a terminar nada bien

}