#include <stdio.h>
#include "cam.h"

int main(int argc, char** argv){
    camInit();

    push();
    quote_val((void*)0);
    cons();
    push();
    void fun1(){
        push();
        push();
        cdr();
        swap();
        quote_int(0);
        cons();
        op_eq();
        void br_true1(){
            quote_int(1);
        }
        void br_false1(){
            push();
            cdr();
            swap();
            push();
            car();
            cdr();
            swap();
            push();
            cdr();
            swap();
            quote_int(1);
            cons();
            op_minus();
            cons();
            app();
            cons();
            op_times();
        }
        branch(br_true1, br_false1);
    }
    cur(fun1);
    swap();
    rplac();
    push();
    cdr();
    swap();
    printf("x= ");
    int a;
    scanf("%d",&a);
    printf("\n%d! = ",a);
    quote_int(a);
    cons();
    app();

    op_print_int();

    return 0;
}
