#include "glue.h"
LLVM_CLANG_C_EXTERN_C_BEGIN
void CXIndexOptions_set_Size(void *instance_ptr, unsigned int value) {
    ((CXIndexOptions *)instance_ptr)->Size = value;
}
void CXIndexOptions_set_ThreadBackgroundPriorityForIndexing(void *instance_ptr, unsigned char value) {
    ((CXIndexOptions *)instance_ptr)->ThreadBackgroundPriorityForIndexing = value;
}
void CXIndexOptions_set_ThreadBackgroundPriorityForEditing(void *instance_ptr, unsigned char value) {
    ((CXIndexOptions *)instance_ptr)->ThreadBackgroundPriorityForEditing = value;
}
void CXIndexOptions_set_ExcludeDeclarationsFromPCH(void *instance_ptr, unsigned value) {
    ((CXIndexOptions *)instance_ptr)->ExcludeDeclarationsFromPCH = value;
}
void CXIndexOptions_set_DisplayDiagnostics(void *instance_ptr, unsigned value) {
    ((CXIndexOptions *)instance_ptr)->DisplayDiagnostics = value;
}
void CXIndexOptions_set_StorePreamblesInMemory(void *instance_ptr, unsigned value) {
    ((CXIndexOptions *)instance_ptr)->StorePreamblesInMemory = value;
}
void CXIndexOptions_set_PreambleStoragePath(void *instance_ptr, const char * value) {
    ((CXIndexOptions *)instance_ptr)->PreambleStoragePath = value;
}
void CXIndexOptions_set_InvocationEmissionPath(void *instance_ptr, const char * value) {
    ((CXIndexOptions *)instance_ptr)->InvocationEmissionPath = value;
}
LLVM_CLANG_C_EXTERN_C_END
