#include "test_namespace.h"

namespace other_ns {

namespace inner {

class TheBase {
public:
    virtual void Update();
    virtual void End();
};

};

};

NS_AX_BEGIN

class CharStorage {
private:
    char *value;
    int size;
public:
    static const int OBJECT_ID = 0x777;
    static constexpr float FPS_TIME = 10.f;
    static constexpr const char kBuff[] = "1234";
    static constexpr int kInts[] = { 1, 2, 3, 4 };
    static constexpr char kSep = '\\';
    static inline const float *kFloats = (const float []){1e0, 1e1, 1e2};
    static int kStaticNumber;
    int OJECT_ID2 = 0x777;

    CharStorage(const char *new_value, int new_size);
    ~CharStorage();

    int getSize() const;
    char *getValue();
};

struct Normal {
    int h;
    int x;
    int d;

    Normal add(const Normal &other);
    Normal sub(const Normal &other);
};

class TheBase {
public:
    virtual void Update();
    virtual void End();
};

class TheChild : public other_ns::inner::TheBase {
public:
    void Update() override;
    void End() override;

    void ChildFn();
};

using IamChild = TheChild;

NS_AX_END

int call_main(int argc, char **argv);
