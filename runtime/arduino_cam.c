#include "cam.h"
#include <Arduino.h>

void setOutputPin(){
    pinMode(*((int*)gStackTop->v), OUTPUT);
}

void setInputPin(){
    pinMode(*((int*)gStackTop->v), INPUT);
}

void digitalWriteHigh(){
    digitalWrite(*((int*)gStackTop->v), HIGH);
}

void digitalWriteLow(){
    digitalWrite(*((int*)gStackTop->v), LOW);
}
