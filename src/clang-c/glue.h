#ifndef __ZIG_LLVM_CLANG_C_GLUE_H__
#define __ZIG_LLVM_CLANG_C_GLUE_H__
#include <clang-c/Index.h>
LLVM_CLANG_C_EXTERN_C_BEGIN
void CXIndexOptions_set_Size(void *instance_ptr, unsigned int value);
void CXIndexOptions_set_ThreadBackgroundPriorityForIndexing(void *instance_ptr, unsigned char value);
void CXIndexOptions_set_ThreadBackgroundPriorityForEditing(void *instance_ptr, unsigned char value);
void CXIndexOptions_set_ExcludeDeclarationsFromPCH(void *instance_ptr, unsigned value);
void CXIndexOptions_set_DisplayDiagnostics(void *instance_ptr, unsigned value);
void CXIndexOptions_set_StorePreamblesInMemory(void *instance_ptr, unsigned value);
void CXIndexOptions_set_PreambleStoragePath(void *instance_ptr, const char * value);
void CXIndexOptions_set_InvocationEmissionPath(void *instance_ptr, const char * value);
#ifdef GLUE_CAST
CXIndexOptions *CXIndexOptions_castAsPtr(void *ptr) { return (CXIndexOptions *)ptr; }
CXIndexOptions CXIndexOptions_castAsValue(void *ptr) { return *(CXIndexOptions *)ptr; }
#endif // GLUE_CAST
LLVM_CLANG_C_EXTERN_C_END
#endif // __ZIG_LLVM_CLANG_C_GLUE_H__
