namespace ax {

int var1;
char var2 = 'c';
const float var3 = 4.f;

struct SomeStruct {
    int a;
    int b;
    int c;
} var4, *var5;
SomeStruct var6;
SomeStruct *var7 = &var6;
int (*var8)(void);

typedef void (*FuncType)(int);
FuncType var9;
using FuncType2 = void(*)(int);
FuncType2 var10;
using SS = SomeStruct;
SS var11;

typedef struct {
    int a;
    int b;
    int c;
} TypedefSS;
TypedefSS var12;

class SomeClass {
private:
    int a;
    int b;
    int c;
};
SomeClass var13;

using SC = SomeClass;
SC var14;

using SC2 = SC;
SC2 var15;

}
