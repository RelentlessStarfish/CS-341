#include <stdio.h>
#include <stdlib.h>

#define NUM 257

/*
    This is an array structiure which can be dynamically used
    with the functions provided below. All that has to be done is
    create a Array** and set it equal it the initilize function of
    that Array's name.
*/

/* The reason that everything is double pointered
    is so that the array can be passed around within function and can
    be directly passed by reference. The resason that a struct was used
    is that instead of being limited by the size of a normal integer, the
    size of the created number can be as large the memory of the computer
    will allow.
*/

/*
    The features of this project include:
        - the ability to use any size numbers in almost every case
        - the symbol "." causes a concatenation of the numbers to
            its left and to its right (left-associative)
        - the symbol "^" causes a repetition of the number on the left
            a number of times equal to the number on the right (right-associative)
        - the "()" causes a the expression inside to be of the highest precidence
        - the precidence, highest to lowest, is "()," "^," and "." for this program
*/

/*
    The limitations of this project include:
        - if the number after the ^ is larger than an integer the
            program will break.
        - the program will only work with real non-negative integers
        - there are memory leaks somewhere
*/

typedef struct{
    int* value;
    int length;
} Array;

int currentToken;
Array** currentAttribute;

void match(int);
int getToken();
Array** concatenator();
void concatenate(Array**, Array**);
Array** repeater();
void repeat(Array**, int);
Array** standard();
void error(char*);

Array** initializeArray(Array** array);
void addArrayElement(Array** array, int element);
void copyArrayElements(Array** arrayTo, Array** arrayFrom);
int arrayToInt(Array** array);
void printArray(Array** array);

Array** initializeArray(Array** array){
    array = (Array**) malloc(sizeof(Array*));
    *array = (Array*) malloc(sizeof(Array));
    (*array)->length = malloc(sizeof(int));
    (*array)->value = (int*) malloc(0*sizeof(int));
    (*array)->length = 0;
    return array;
}

void addArrayElement(Array** array, int element){
    int length = (*array)->length + 1;

    (*array)->length = length;
    (*array)->value = (int*) realloc((*array)->value, length * sizeof(int));
    (*array)->value[length - 1] = element;
}

void copyArrayElements(Array** arrayTo, Array** arrayFrom){
    int lengthFrom = (*arrayFrom)->length;

    for(int counter = 0; counter < lengthFrom; ++counter){
        addArrayElement(arrayTo, (*arrayFrom)->value[counter]);
    }
}

int arrayToInt(Array** array){
    int length = (*array)->length;
    int value = 0;

    if(length != 0){
        value = (*array)->value[0];
    }

    for(int counter = 1; counter < length; ++counter){
        value *= 10;
        value += (*array)->value[counter];
    }

    return value;
}

void printArray(Array** array){
    int length = (*array)->length;

    for(int counter = 0; counter < length; ++counter){
        fprintf(stderr,"%i", (*array)->value[counter]);
    }
    fprintf(stderr, "\n\n");
}

int main(){
    Array** value = initializeArray(value);


    /* Testing Code

    Array** temp4 = initializeArray(temp4);
    addArrayElement(temp4, 3);

    Array** temp5 = initializeArray(temp5);
    addArrayElement(temp5, 2);

    copyArrayElements(temp5, temp4);

    repeat(temp5, 2);
    printArray(temp5);
    */

    currentToken = getToken();
    while(1){
        value = concatenator();
        printArray(value);
        match('\n');
    }
    return 0;
}

/* recursive-descent parser for the following grammar:
 *  C: Concatenation, R: Repeater, S: Standard
 *
 *	C -> C '+' R | R
 *  R -> R '*' S | S
 *  S -> '(' C ')' | NUM
 */

/* handler for the rule:
 *  C -> C '+' R | R
 * fixing the left-recursion problem:
 *  C -> R C'
 *  C' -> '+' R C' | empty
 */

Array** concatenator(){
    Array** value = repeater();
    while(currentToken == '.'){
        match('.');
        concatenate(value, repeater());
    }
    return value;
}

void concatenate(Array** value, Array** secondValue){
    copyArrayElements(value, secondValue);
}

/* handler for the rule:
 *  R -> S '^' R | S
 * fixing the right-recursion problem:
 *  R -> C' S
 *  C' -> '^' R C' | empty
 */

Array** repeater(){
    Array** value = standard();
    if(currentToken == '^'){
        match('^');
        int repetitions = arrayToInt(standard());
        repeat(value, repetitions);
    }
    return value;
}

void repeat(Array** array, int repetitions){
    Array** temp = initializeArray(temp);
    copyArrayElements(temp, array);

    for (int counter = 1; counter < repetitions; ++counter){
        copyArrayElements(array, temp);
    }

    free(temp);
}

/* handler for the rule:
 *  S -> '(' C ')' | NUM
 */

Array** standard(){
    Array** value;

    if (currentToken == '('){
        match('(');
        value = concatenator();
        match(')');
    }else if(currentToken == NUM) {
        value = currentAttribute;
        match(NUM);
    }else{
        error("Illegal token in Factor");
    }
    return value;
}

/* match */

void match( int expectedToken){
    if(currentToken == expectedToken){
        currentToken = getToken();
    }else{
        error("Unexpected token");
    }
}

/* scanner (lexical analyzer) or tokenizer for a simple calculator */

int getToken(){
    int character;
    Array** value;

    while(1){
        switch(character = getchar()){
        /* operators */
        case '^':
            fprintf( stderr, "[OP:%c]", character);
            return character;
        case '.':
            fprintf(stderr, "[OP:%c]", character);
            return character;
        /* brackets */
        case '(': case ')':
            fprintf(stderr, "[OP:%c]", character);
            return character;
        /* whitespace */
        case ' ': case '\t':
            continue;
        /* newline */
        case '\n':
            fprintf(stderr, "%c", character);
            return character;
        /* the rest */
        default:
            /* numbers */
            if(isdigit(character)){
                value = initializeArray(value);
                do {
                    addArrayElement(value, (character - '0'));
                } while (isdigit(character = getchar()));
                ungetc(character, stdin);
                fprintf(stderr, "[NUM:%d]", value);
                currentAttribute = value;
                return NUM;
            }
            else error("Illegal character");
        }
    }
}

/* simple error handler */
void error(char *message){
    fprintf(stderr, "Error %s\n", message);
    exit(1);
}


