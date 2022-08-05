typedef enum { IntPtr, BoolPtr, SegDes, String } EntryType; 
typedef struct {
    EntryType type;

    union {
        int *intPtr;
        bool *boolPtr;
        int *segDes;
        char *str;
    };
    int length;
} StackEntry;

class Stack {
    public:
    // Deletes i elements starting from position j within the stack and compresses.
    void Pop(int i, int j);

    // Combined Copy+Pop. i elements, starting at j, are moved to the top of the stack.
    void CPop (int i, int j);

    // Copy i elements starting from position j to the top of the stack.
    void Copy(int i, int j);

    char *PopString(int &length);
    int *PopIntPtr(int &length);
    int *PopSegDes(int &length);

    void PushIntPtr(int *p, int length);
    void PushBool(bool b);
    void PushString(char *p, int length);
    void PushSegDes(int *p, int length);
    void PushInt(int i);

    // For testing purposes only!
    int PopInt();

    Stack();

    private:
    void CheckStackSpace(int n);
    void Swap(int i, int j);
    int nEntries;
    int totalEntries;
    StackEntry *entries;
};
    
extern Stack *g_Stack;

extern "C" void VectorMultiplyInts();
extern "C" void VectorAddInts();
extern "C" void DistributeIntegers();
extern "C" void Index();
extern "C" void Lengths();
extern "C" void MakeSegDes();
extern "C" void DPermute();
