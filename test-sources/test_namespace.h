
#ifdef __cplusplus
#    define NS_AX_BEGIN \
        namespace ax    \
        {
#    define NS_AX_END }
#    define USING_NS_AX using namespace ax
#    define NS_AX ::ax
#else
#    define NS_AX_BEGIN
#    define NS_AX_END
#    define USING_NS_AX
#    define NS_AX
#endif
