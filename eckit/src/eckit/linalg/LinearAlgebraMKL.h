/*
 * (C) Copyright 1996-2017 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @file   LinearAlgebraMKL.h
/// @author Florian Rathgeber
/// @date   June 2015

#ifndef eckit_la_LinearAlgebraMKL_h
#define eckit_la_LinearAlgebraMKL_h

#include "eckit/eckit_config.h"

#ifdef ECKIT_HAVE_MKL

#include "eckit/linalg/LinearAlgebra.h"

namespace eckit {
namespace linalg {

//-----------------------------------------------------------------------------

class LinearAlgebraMKL : public LinearAlgebra {

public:
    LinearAlgebraMKL();

private:

    // Overridden methods

    virtual Scalar dot(const Vector&, const Vector&) const;
    virtual void gemv(const Matrix&, const Vector&, Vector&) const;
    virtual void gemm(const Matrix&, const Matrix&, Matrix&) const;
    virtual void spmv(const SparseMatrix&, const Vector&, Vector&) const;
    virtual void spmm(const SparseMatrix&, const Matrix&, Matrix&) const;
    virtual void dsptd(const Vector&, const SparseMatrix&, const Vector&, SparseMatrix&) const;

    virtual void print(std::ostream&) const;

};

//-----------------------------------------------------------------------------

}  // namespace linalg
} // namespace eckit

#endif // HAVE_MKL

#endif
