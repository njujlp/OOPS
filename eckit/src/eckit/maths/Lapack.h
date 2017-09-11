#ifndef eckit_maths_lapack_h
#define eckit_maths_lapack_h

#include "eckit/eckit_config.h"

namespace eckit {
namespace maths {
namespace lapack {

void getrf(int* M, int* N, float* data, int* lda, int* ipiv, int* info);
void getrf(int* M, int* N, double* data, int* lda, int* ipiv, int* info);

void getri(int* M, float* data, int* lda, int* ipiv, float* work, int* lwork, int* info);
void getri(int* M, double* data, int* lda, int* ipiv, double* work, int* lwork, int* info);

} // namespace lapack
} // namespace maths
} // namespace eckit

#endif
