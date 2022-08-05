#include "ndp2gpu.h"

void fun1() {
  g_Stack->PushInt(10);
  int *ints112 = g_Stack->PopIntPtr();
  int *result113 = MakeSegDes(ints112);
  g_Stack->PushSegDes(result113);
  g_Stack->PushString("Running..\n");
  fun4();
  g_Stack->Pop(1, 0);
  g_Stack->PushInt(1);
  g_Stack->PushInt(1);
  fun5();
  g_Stack->PushInt(2);
  fun6();
  g_Stack->PushInt(3);
  fun6();
  g_Stack->PushInt(4);
  fun6();
  g_Stack->Copy(1, 1);
  g_Stack->CPop(1, 2);
  g_Stack->CPop(1, 2);
  g_Stack->PushInt(2);
  g_Stack->Copy(1, 3);
  fun7();
  g_Stack->CPop(1, 3);
  g_Stack->Pop(1, 0);
  int *arg114 = g_Stack->PopIntPtr();
  int *arg115 = g_Stack->PopIntPtr();
  int *result116 = VectorMultiplyInts(arg1, arg2);
  g_Stack->PushIntPtr(result116);
  g_Stack->Copy(1, 1);
  g_Stack->CPop(1, 2);
  g_Stack->CPop(1, 2);
  g_Stack->PushInt(1);
  g_Stack->Copy(1, 3);
  fun7();
  g_Stack->CPop(1, 3);
  g_Stack->Pop(1, 0);
  int *arg117 = g_Stack->PopIntPtr();
  int *arg118 = g_Stack->PopIntPtr();
  int *result119 = VectorAddInts(arg1, arg2);
  g_Stack->PushIntPtr(result119);
  g_Stack->PushInt(9);
  int *ints120 = g_Stack->PopIntPtr();
  int *result121 = MakeSegDes(ints120);
  g_Stack->PushSegDes(result121);
  g_Stack->PushString("Exiting..");
  fun4();
  g_Stack->Pop(1, 0);
  }
void fun16() {
  int *ints122 = g_Stack->PopIntPtr();
  int *result123 = MakeSegDes(ints122);
  g_Stack->PushSegDes(result123);
  g_Stack->Copy(1, 0);
  g_Stack->CPop(1, 2);
  g_Stack->CPop(1, 2);
  fun7();
  }
void fun5() {
  int *ints124 = g_Stack->PopIntPtr();
  int *result125 = MakeSegDes(ints124);
  g_Stack->PushSegDes(result125);
  g_Stack->Copy(1, 0);
  g_Stack->CPop(1, 2);
  g_Stack->CPop(1, 2);
  fun7();
  }
void fun6() {
  g_Stack->CPop(2, 1);
  g_Stack->CPop(1, 2);
  g_Stack->PushInt(1);
  fun16();
  fun21();
  }
void fun22() {
  g_Stack->Pop(1, 0);
  int *segdes126 = g_Stack->PopSegDes();
  int *result127 = Lengths(segdes126);
  g_Stack->PushIntPtr(result127);
  }
void fun25() {
  g_Stack->CPop(2, 4);
  g_Stack->CPop(2, 4);
  g_Stack->Pop(1, 1);
  g_Stack->CPop(2, 3);
  g_Stack->Copy(1, 1);
  g_Stack->CPop(1, 4);
  g_Stack->CPop(1, 4);
  g_Stack->CPop(1, 3);
  g_Stack->CPop(1, 5);
  g_Stack->CPop(1, 5);
  int *data128 = g_Stack->PopIntPtr();
  int *index129 = g_Stack->PopIntPtr();
  int *default130 = g_Stack->PopIntPtr();
  int *srcseg131 = g_Stack->PopSegDes();
  int *dstseg132 = g_Stack->PopSegDes();
  int *result133 = DPermute(data128, index129, default130, srcseg131, dstseg132);
  g_Stack->PushIntPtr(result133);
  }
void fun32() {
  g_Stack->Pop(1, 0);
  g_Stack->Copy(1, 0);
  g_Stack->CPop(1, 3);
  g_Stack->CPop(1, 3);
  g_Stack->CPop(1, 3);
  int *values134 = g_Stack->PopIntPtr();
  int *strides135 = g_Stack->PopIntPtr();
  int *segdes136 = g_Stack->PopSegDes();
  int *result137 = Index(values134, strides135, segdes136);
  g_Stack->PushIntPtr(result137);
  }
void fun7() {
  g_Stack->CPop(1, 1);
  g_Stack->CPop(1, 1);
  int *ints138 = g_Stack->PopIntPtr();
  int *segdes139 = g_Stack->PopSegDes();
  int *result140 = DistributeIntegers(ints138, segdes139);
  g_Stack->PushIntPtr(result140);
  }
void fun40() {
  g_Stack->Copy(2, 6);
  g_Stack->CPop(2, 6);
  g_Stack->Copy(2, 6);
  g_Stack->CPop(2, 6);
  g_Stack->CPop(2, 10);
  fun22();
  g_Stack->CPop(2, 9);
  fun22();
  int *arg141 = g_Stack->PopIntPtr();
  int *arg142 = g_Stack->PopIntPtr();
  int *result143 = VectorAddInts(arg1, arg2);
  g_Stack->PushIntPtr(result143);
  fun44();
  fun25();
  }
void fun44() {
  g_Stack->CPop(2, 3);
  g_Stack->CPop(2, 3);
  g_Stack->PushInt(0);
  g_Stack->CPop(1, 5);
  fun16();
  fun25();
  }
void fun21() {
  g_Stack->Copy(2, 2);
  g_Stack->PushInt(0);
  g_Stack->PushInt(1);
  g_Stack->Copy(2, 6);
  fun32();
  g_Stack->Copy(2, 4);
  g_Stack->CPop(2, 8);
  fun22();
  g_Stack->PushInt(1);
  g_Stack->CPop(2, 8);
  fun32();
  fun40();
  }
void fun4() {
  g_Stack->Pop(1, 1);
  char *pstr144 = g_Stack->PopString();
  printf(pstr144);
  g_Stack->Pop(2, 0);
  g_Stack->PushBool(1);
  }
