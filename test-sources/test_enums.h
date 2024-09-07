namespace ax {

enum Enums {
    Enum1 = 1,
    Enum2 = 2,
    Enum3 = Enum1,
} i_am_a_var = Enum1;

enum class EnumsClass {
    Enum4 = 3,
    Enum5 = 4,
    Enum6 = Enum4,
};

enum class EnumsClassChar : char {
    Enum7 = 5,
    Enum8 = 6,
    Enum9 = Enum7,
};

enum class BoolEnum : bool {
    EnumFalse = false,
    EnumTrue = true,
};

enum ClassicEnum {
    VariantEnum1,
    VariantEnum2,
    VariantEnum3 = VariantEnum1,
};

}
