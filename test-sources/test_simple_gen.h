#ifndef _SIMPLE_GEN_H_
#define _SIMPLE_GEN_H_

#include <vector>

using IntVector = std::vector<int>;

class Base {
private:
    int inner_int1_;
    bool inner_flag1_;
    std::vector<int> inner_int_vec1_;

public:
    std::vector<int> outer_int_vec1_;
    bool Init();

    int GetInnerInt1() const;
    void SetInnerInt2(int);

    bool IsFlag1() const;

    virtual void VirMethod1();
    virtual void PureVirMethod1() = 0;

    void SetIntVec1(const std::vector<int>&);
};

class Child : public Base {
private:
    float inner_float1_;
public:
    Child(float t_inner_float) : inner_float1_(t_inner_float) {
        Base::Init();
    }

    float GetInnerFloat1() const;
    float GetInnerFloat1(float default_value) const;
    void SetInnerFloat1(float);

    void VirMethod1() override;
    void PureVirMethod1() override;
};

int TestSimpleGenFunc1();

#endif // _SIMPLE_GEN_H_
