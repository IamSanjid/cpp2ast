namespace ax {

class Class1 {
};
typedef Class1 TypeDef1;
using TypeDef2 = Class1;

enum Enum1 {
    Variant1,
    Variant2,
};
typedef Enum1 TypeDef3;

TypeDef2 Class1Var1;
TypeDef1 *Class1Ptr1;
TypeDef1 **Class1Ptr2;
TypeDef1 ***Class1Ptr3;

}
