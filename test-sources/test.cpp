#include "test.h"

NS_AX_BEGIN

CharStorage::CharStorage(const char *new_value, int new_size) 
    : value(new char[new_number]), size(new_number)
{
    for (int i = 0; i < new_size; i++) {
        value[i] = new_value[i];
    }
}

CharStorage::~CharStorage() { delete[] value; }

int Test::getSize() const {
    return size;
}

char *getValue() {
    return value;
}

const int CharStorage::kStaticNumber = 54321;

NS_AX_END
