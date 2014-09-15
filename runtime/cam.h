#ifndef CAM_H
#define CAM_H

#include <stdlib.h>
#include <gc.h>

struct pair{
    void* first;
    void* second;
};
struct stack_item{
    void* v;
    struct stack_item* next;
};

struct stack_item* gStackTop;

void quote_val(void* v);

void quote_int(int v);

void car();

void cdr();

void cons();

void push();

void swap();

void branch(void (*br_true)(void), void(*br_false)(void));

void cur(void (*c)(void));

void app();

void rplac();

void op_minus();

void op_times();

void op_eq();

void op_print_int();

void camInit();

#endif //CAM_H
