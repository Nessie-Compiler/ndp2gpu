#include "ndp2gpu.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

// NOTES:
// This code only uses asserts around what might otherwise be dynamic checks, as it is
// assumed that all generated VCODE is dynamically safe. This is not intended to be an
// externally-useful stack class!

Stack *g_Stack = new Stack();


// Entries are an array of items. The "top" of the stack is at the higher
// end of the array. So, Pop(1,0) -- remove the top item from the stack --
// operates as nEntries--;
Stack::Stack () {
    nEntries = 0;
    totalEntries = 0;
    entries = NULL;
}

void Stack::CheckStackSpace(int n) {
    if (nEntries + n > totalEntries) {
        int newSize = totalEntries * 2;

        if (newSize < 256)
            newSize = 256;

        while (newSize <= nEntries + n)
            newSize *= 2;
        
        StackEntry *newEntries = (StackEntry*)malloc(newSize*sizeof(StackEntry));
        memcpy(entries, newEntries, nEntries * sizeof(StackEntry));
        free (entries);
        entries = newEntries;
        totalEntries = newSize;        
    }
}

void Stack::Swap(int i, int j) {
    assert(i < nEntries);
    assert(j < nEntries);
    assert(i != j);
    
    //    printf (" Swap: %d, %d\n", i, j);
    StackEntry temp;
    memcpy(&temp, &entries[i], sizeof(StackEntry));
    memcpy(&entries[i], &entries[j], sizeof(StackEntry));
    memcpy(&entries[j], &temp, sizeof(StackEntry));
}

// Deletes i elements starting from position j within the stack and compresses.
void Stack::Pop (int i, int j) {
    assert (nEntries >= (j+i));
    //    printf ("Popping %d, %d\n", i, j);

    CPop (i, j);

    nEntries -= i;
}

// Combined Copy+Pop. i elements, starting at j, are moved to the top of the stack.
void Stack::CPop (int i, int j) {
    assert (nEntries >= (j+i));
    //    printf ("CPopping %d, %d\n", i, j);
    
    for (int shift = 0; shift < i; shift++) {
        for (int next = (nEntries-1-j); next < nEntries-1; next++) {
            Swap (next, next+1);
        }
    }
}

// Copy i elements starting from position j to the top of the stack.
void Stack::Copy(int i, int j) {
    assert (nEntries > j);
    assert (j+1 >= i);
    
    CheckStackSpace(i);

    memcpy(&entries[nEntries], &entries[nEntries-1-j], i*sizeof(StackEntry));
    nEntries += i;
}

char *Stack::PopString(int &length) {
    assert (nEntries > 0);
    assert (entries[nEntries-1].type == String);
    nEntries--;
    length = entries[nEntries].length;
    return entries[nEntries].str;
}

int *Stack::PopIntPtr(int &length) {
    assert (nEntries > 0);
    assert (entries[nEntries-1].type == IntPtr);
    nEntries--;
    length = entries[nEntries].length;
    return entries[nEntries].intPtr;
}

int *Stack::PopSegDes(int &length) {
    assert (nEntries > 0);
    assert (entries[nEntries-1].type == SegDes);
    
    nEntries--;
    length = entries[nEntries].length;
    return entries[nEntries].segDes;
}

int Stack::PopInt() {
    assert (nEntries > 0);
    assert (entries[nEntries-1].type == IntPtr);
    assert (entries[nEntries-1].length == 1);
    
    nEntries--;
    return entries[nEntries].intPtr[0];
}

void Stack::PushIntPtr(int *p, int length) {
    CheckStackSpace(1);

    entries[nEntries].type = IntPtr;
    entries[nEntries].intPtr = p;
    entries[nEntries].length = length;
    nEntries++;
}

void Stack::PushString(char *p, int length) {
    CheckStackSpace(1);

    entries[nEntries].type = String;
    entries[nEntries].str = p;
    entries[nEntries].length = length;
    nEntries++;
}

void Stack::PushSegDes(int *p, int length) {
    CheckStackSpace(1);

    entries[nEntries].type = SegDes;
    entries[nEntries].segDes = p;
    entries[nEntries].length = length;
    nEntries++;
}

void Stack::PushBool(bool b) {
    CheckStackSpace(1);

    entries[nEntries].type = BoolPtr;
    entries[nEntries].length = 1;
    entries[nEntries].boolPtr = (bool*)malloc(sizeof(bool));
    entries[nEntries].boolPtr[0] = b;
    nEntries++;
}

void Stack::PushInt(int i) {
    CheckStackSpace(1);

    entries[nEntries].type = IntPtr;
    entries[nEntries].length = 1;
    entries[nEntries].intPtr = (int*)malloc(sizeof(int));
    entries[nEntries].intPtr[0] = i;
    nEntries++;
}
