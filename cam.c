#include "cam.h"
#include <stdio.h>

void quote_val(void* v){
    gStackTop->v = v;
}
void quote_int(int v){
    int* intp = malloc(sizeof(int));
    *intp = v;
    gStackTop->v = intp;
}
void car(){
    gStackTop->v = ((struct pair*)gStackTop->v)->first;
}
void cdr(){
    gStackTop->v = ((struct pair*)gStackTop->v)->second;
}
void cons(){
    struct pair* p;
    p = (struct pair*) malloc(sizeof(struct pair));
    p->first = gStackTop->next->v;
    p->second = gStackTop->v;
    gStackTop = gStackTop->next;
    gStackTop->v = (void*)p;
}
void push(){
    struct stack_item* p;
    p = (struct stack_item*)malloc(sizeof(struct stack_item));
    p->v = gStackTop->v;
    p->next = gStackTop;
    gStackTop = p;
}
void swap(){
    void* tmp = gStackTop->v;
    gStackTop->v = gStackTop->next->v;
    gStackTop->next->v = tmp;
}
void branch(void (*br_true)(void), void(*br_false)(void)){
    if(*((int*)gStackTop->v)){
        gStackTop = gStackTop->next;
        br_true();
    }
    else{
        gStackTop = gStackTop->next;
        br_false();
    }
}
void cur(void (*c)(void)){
    struct pair* closure;
    closure = (struct pair*) malloc(sizeof(struct pair));
    closure->first = c;
    closure->second = gStackTop->v;

    gStackTop->v = (void*)closure;
}
void app(){
    void(*c)(void) = (void*)
        ((struct pair*)((struct pair*)gStackTop->v)->first)->first;
    ((struct pair*)gStackTop->v)->first =
        ((struct pair*)((struct pair*)gStackTop->v)->first)->second;
    c();
}
void rplac(){
    struct pair* p1 = (struct pair*) gStackTop->next->v;
    ((struct pair*)gStackTop->v)->second = (void*) p1;
    gStackTop->next = gStackTop->next->next;
    ((struct pair*)p1->second)->second = (void*) p1;
}

void op_minus(){
    int a = *((int*)((struct pair*)gStackTop->v)->first);
    int b = *((int*)((struct pair*)gStackTop->v)->second);

    int* intp = (int*) malloc(sizeof(int));
    *intp = (a-b);
    gStackTop->v = intp;
}
void op_times(){
    int a = *((int*)((struct pair*)gStackTop->v)->first);
    int b = *((int*)((struct pair*)gStackTop->v)->second);

    int* intp = (int*) malloc(sizeof(int));
    *intp = (a*b);
    gStackTop->v = intp;
}
void op_eq(){
    int a = *((int*)((struct pair*)gStackTop->v)->first);
    int b = *((int*)((struct pair*)gStackTop->v)->second);

    int* intp = (int*) malloc(sizeof(int));
    *intp = (a==b);
    gStackTop->v = intp;
}

void op_print_int(){
    printf("%d\n", *((int*)gStackTop->v));
}

void camInit(){
    gStackTop = (struct stack_item*) malloc(sizeof(struct stack_item));
    gStackTop->v = NULL;
    gStackTop->next = NULL;
}

