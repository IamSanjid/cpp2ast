namespace ax {
    int Method1(void);
    typedef int (*MethodTypedef1)(int);
    using MethodTypedef2 = MethodTypedef1;
    MethodTypedef1 VarMethod1;
    MethodTypedef2 VarMethod2;
};
