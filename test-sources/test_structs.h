namespace ax {

struct Struct1 {
    int Field1;
    int Field2;
    int Field3 = 0;
    struct Struct3 *Field4;
    struct NonExisting1 *Field5;
};

typedef struct Struct2 {
    int Field1;
    Struct1 Field2;
    Struct2 *Field3;
} Struct2;

struct Struct3 {
    Struct2 Field1;
    void Method0();
    void Method1(int);
    void Method2(int*);
    void Method3(int&);
    void Method4(int&&);
    void Method5(Struct2);
    void Method6(Struct2*);
    void Method7(Struct2&);
    Struct2 Method8();
    Struct2* Method9() {
        return &Field1;
    }
    Struct2& Method10();

    static void StaticMethod1(int);

    Struct2* operator->();
};

}
