/*
 * (C) Copyright 1996-2017 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

//-----------------------------------------------------------------------------

#include "eckit/eckit_config.h"

#ifdef ECKIT_HAVE_ARMADILLO

#include "eckit/linalg/LinearAlgebraArmadillo.h"

#include <armadillo>

#include "eckit/exception/Exceptions.h"
#include "eckit/linalg/Matrix.h"
#include "eckit/linalg/Vector.h"

//-----------------------------------------------------------------------------

namespace eckit {
namespace linalg {

//-----------------------------------------------------------------------------

LinearAlgebraArmadillo::LinearAlgebraArmadillo() : LinearAlgebra("armadillo") {}

//-----------------------------------------------------------------------------

void LinearAlgebraArmadillo::print(std::ostream& out) const {
    out << "LinearAlgebraArmadillo[]";
}

//-----------------------------------------------------------------------------

Scalar LinearAlgebraArmadillo::dot(const Vector& x, const Vector& y) const {
    ASSERT( x.size() == y.size() );
    // Armadillo requires non-const pointers to the data for views without copy
    arma::vec xi(const_cast<Scalar*>(x.data()), x.size(), /* copy_aux_mem= */ false);
    arma::vec yi(const_cast<Scalar*>(y.data()), y.size(), /* copy_aux_mem= */ false);
    return arma::dot(xi, yi);
}

//-----------------------------------------------------------------------------

void LinearAlgebraArmadillo::gemv(const Matrix& A, const Vector& x, Vector& y) const {
    ASSERT( x.size() == A.cols() && y.size() == A.rows() );
    // Armadillo requires non-const pointers to the data for views without copy
    arma::mat Ai(const_cast<Scalar*>(A.data()), A.rows(), A.cols(), /* copy_aux_mem= */ false);
    arma::vec xi(const_cast<Scalar*>(x.data()), x.size(), /* copy_aux_mem= */ false);
    arma::vec yi(y.data(), y.size(), /* copy_aux_mem= */ false);
    yi = Ai*xi;
}

//-----------------------------------------------------------------------------

void LinearAlgebraArmadillo::gemm(const Matrix& A, const Matrix& B, Matrix& C) const {
    ASSERT( A.cols() == B.rows() && A.rows() == C.rows() && B.cols() == C.cols() );
    // Armadillo requires non-const pointers to the data for views without copy
    arma::mat Ai(const_cast<Scalar*>(A.data()), A.rows(), A.cols(), /* copy_aux_mem= */ false);
    arma::mat Bi(const_cast<Scalar*>(B.data()), B.rows(), B.cols(), /* copy_aux_mem= */ false);
    arma::mat Ci(C.data(), C.rows(), C.cols(), /* copy_aux_mem= */ false);
    Ci = Ai*Bi;
}

//-----------------------------------------------------------------------------

void LinearAlgebraArmadillo::spmv(const SparseMatrix& A, const Vector& x, Vector& y) const {
    // FIXME: Armadillo stores matrices in CSC format and does not provide
    // constructors from existing storage. A sparse matrix would have to be
    // copied from CSR to CSC format, which is probably not worth the cost
    LinearAlgebra::getBackend("generic").spmv(A, x, y);
}

//-----------------------------------------------------------------------------

void LinearAlgebraArmadillo::spmm(const SparseMatrix& A, const Matrix& B, Matrix& C) const {
    // FIXME: Armadillo stores matrices in CSC format and does not provide
    // constructors from existing storage. A sparse matrix would have to be
    // copied from CSR to CSC format, which is probably not worth the cost
    LinearAlgebra::getBackend("generic").spmm(A, B, C);
}

//-----------------------------------------------------------------------------

void LinearAlgebraArmadillo::dsptd(const Vector& x, const SparseMatrix& A, const Vector& y, SparseMatrix& B) const {
    LinearAlgebra::getBackend("generic").dsptd(x, A, y, B);
}

//-----------------------------------------------------------------------------

static LinearAlgebraArmadillo LinearAlgebraArmadillo;

//-----------------------------------------------------------------------------

}  // namespace linalg
} // namespace eckit

#endif  // ECKIT_HAVE_ARMDILLO
