#include <stdlib.h>
#include <stdio.h>

#include "ndp2gpu.h"


void TestSimplePop () {
    g_Stack->PushInt(1);
    g_Stack->PushInt(2);
    g_Stack->PushInt(3);

    g_Stack->Pop(1,1);
    
    int i = g_Stack->PopInt();
    if (i != 3) {
        printf ("Pop First: expected 3, got %d\n", i);
    }
    
    i = g_Stack->PopInt();
    if (i != 1) {
        printf ("Pop Second: expected 1, got %d\n", i);
    }
}

void TestSimpleCPop () {
    g_Stack->PushInt(1);
    g_Stack->PushInt(2);
    g_Stack->PushInt(3);

    g_Stack->CPop(1,1);

    int i = g_Stack->PopInt();
    if (i != 2) {
        printf ("CPop First: expected 2, got %d\n", i);
    }

    i = g_Stack->PopInt();
    if (i != 3) {
        printf ("CPop Second: expected 3, got %d\n", i);
    }    

    i = g_Stack->PopInt();
    if (i != 1) {
        printf ("CPop Third: expected 1, got %d\n", i);
    }    
}

void TestMorePop () {
    for (int i = 20; i >= 0; i--) {
        g_Stack->PushInt(i);
    }

    g_Stack->Pop(5, 15);

    for (int i = 1; i < 15; i++) {
        printf ("%d ", g_Stack->PopInt());
    }
    printf("\n");
        
}

void TestCopy () {
    for (int i = 5; i >= 0; i--) {
        g_Stack->PushInt(i);
    }

    g_Stack->Copy(3, 2);

    for (int i = 1; i < 9; i++) {
        printf ("%d ", g_Stack->PopInt());
    }
    printf("\n");
        
}

int main (void) {
    TestSimplePop();
    TestSimpleCPop();
    TestMorePop();
    TestCopy();
}
