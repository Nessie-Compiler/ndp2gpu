#include "ndp2gpu.h"
#include <stdlib.h>
#include <assert.h>

void VectorMultiplyInts() {
    int secondLen;
    int *second = g_Stack->PopIntPtr(secondLen);
    int firstLen;
    int *first = g_Stack->PopIntPtr(firstLen);

    // TODO: ref counting stuff.

    int *foo = (int*)malloc(firstLen * sizeof(int));
    for (int i = 0; i < firstLen; i++)
        foo[i] = first[i]*second[i];

    g_Stack->PushIntPtr (foo, firstLen);
}

void VectorAddInts() {
    int secondLen;
    int *second = g_Stack->PopIntPtr(secondLen);
    int firstLen;
    int *first = g_Stack->PopIntPtr(firstLen);

    int *foo = (int*)malloc(firstLen * sizeof(int));
    for (int i = 0; i < firstLen; i++)
        foo[i] = first[i]+second[i];

    g_Stack->PushIntPtr (foo, firstLen);
}

void DistributeIntegers() {
    int segdesLen;
    int *segdes = g_Stack->PopSegDes(segdesLen);
    int valuesLen;
    int *values = g_Stack->PopIntPtr(valuesLen);

    int totalLen = 0;
    for (int i = 0; i < segdesLen; i++)
        totalLen += segdes[i];

    int *foo = (int*)malloc(totalLen * sizeof(int));
    int n = 0;
    for (int i = 0; i < segdesLen; i++) {
        for (int j = 0; j < segdes[i]; j++) {
            foo[n] = values[i];
            n++;
        }
    }    

    g_Stack->PushIntPtr (foo, totalLen);
}

void Index() {
    int segdesLen;
    int *segdes = g_Stack->PopSegDes(segdesLen);
    int stridesLen;
    int *strides = g_Stack->PopIntPtr(stridesLen);
    int initLen;
    int *init = g_Stack->PopIntPtr(initLen);


    int totalLen = 0;
    for (int i = 0; i < segdesLen; i++)
        totalLen += segdes[i];

    int *foo = (int*)malloc(totalLen * sizeof(int));

    int *dest = foo;
    int i, seg;
    int curr;
    for (seg = 0; seg < segdesLen; seg++) {
        int this_stride = ((int *)strides)[seg];
        int this_init = ((int *)init)[seg];
        int elts_in_seg = ((int *)segdes)[seg];
        curr = this_init;
        for (i = 0; i < elts_in_seg; i++) {
            *(dest++) = curr;
            curr += this_stride;
        }                             
    }

    g_Stack->PushIntPtr (foo, totalLen);
}

void Lengths() {
    int segdesLen;
    int *segdes = g_Stack->PopSegDes(segdesLen);

    g_Stack->PushIntPtr (segdes, segdesLen);
}

void MakeSegDes() {
    int valuesLen;
    int *values = g_Stack->PopIntPtr(valuesLen);

    g_Stack->PushSegDes(values, valuesLen);
}

void DPermute() {
    int dstSegLen;
    int *dstSeg = g_Stack->PopSegDes(dstSegLen);
    int srcSegLen;
    g_Stack->PopSegDes(srcSegLen);
    int defLen;
    int *def = g_Stack->PopIntPtr(defLen);
    int indexLen;
    int *index = g_Stack->PopIntPtr(indexLen);
    int dataLen;
    int *data = g_Stack->PopIntPtr(dataLen);
    // TODO

    int totalLen = 0;
    for (int i = 0; i < dstSegLen; i++)
        totalLen += dstSeg[i];

    int *foo = (int*)malloc(totalLen * sizeof(int));

    assert(defLen == totalLen);
    for (int i = 0; i < defLen; i++)
        foo[i] = def[i];

    for (int i = 0; i < indexLen; i++)
        foo[index[i]] = data[i];
    
    g_Stack->PushIntPtr(foo, totalLen);
}
