/*
 * (C) Copyright 2009-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef OOPS_ASSIMILATION_DRIPCGMINIMIZER_H_
#define OOPS_ASSIMILATION_DRIPCGMINIMIZER_H_

#include <cmath>
#include <string>
#include <vector>

#include "util/Logger.h"
#include "oops/assimilation/BMatrix.h"
#include "oops/assimilation/ControlIncrement.h"
#include "oops/assimilation/CostFunction.h"
#include "oops/assimilation/DRMinimizer.h"
#include "oops/assimilation/HtRinvHMatrix.h"
#include "oops/assimilation/QNewtonLMP.h"
#include "util/dot_product.h"
#include "util/formats.h"

namespace oops {

/// Derber-Rosati IPCG Minimizer
/*!
 * \brief Derber-Rosati Inexact-Preconditioned Conjugate Gradients solver.
 *
 * This solver is based on the Golub-Ye Inexact-Preconditioned Conjugate
 * Gradients solver for Ax=b (G.H. Golub and Q. Ye 1999/00,
 * SIAM J. Sci. Comput. 21(4) 1305-1320), and on the Derber and Rosati double
 * PCG algorithm (J. Derber and A. Rosati, 1989, J. Phys. Oceanog. 1333-1347).
 * It solves \f$ Ax=b\f$ for the particular case \f$ A=B^{-1}+C\f$,
 * without requiring the application of \f$ B^{-1}\f$.
 *
 * A must be square, symmetric, positive definite.
 *
 * A preconditioner must be supplied that, given a vector q, returns an
 * approximation to \f$ (AB)^{-1} q\f$. The preconditioner can be variable.
 * Note that the traditional \f$ B\f$-preconditioning corresponds to
 * precond=\f$I\f$.
 *
 * On entry:
 * -    x       =  starting point, \f$ X_0 \f$.
 * -    xh      = \f$ B^{-1} x_0\f$.
 * -    b       = right hand side.
 * -    B       = \f$ B \f$.
 * -    C       = \f$ C \f$.
 * -    precond = preconditioner \f$ F_k \approx (AB)^{-1} \f$.
 *
 * On exit, x will contain the solution \f$ x \f$ and xh will contain
 * \f$ B^{-1} x\f$.
 *  The return value is the achieved reduction in residual norm.
 *
 *  Iteration will stop if the maximum iteration limit "maxiter" is reached
 *  or if the residual norm reduces by a factor of "tolerance".
 *
 *  Each of BMATRIX, CMATRIX and PMATRIX must implement a method:
 *  - void multiply(const VECTOR&, VECTOR&) const
 *
 *  which applies the matrix to the first argument, and returns the
 *  matrix-vector product in the second. (Note: the const is optonal, but
 *  recommended.)
 */

// -----------------------------------------------------------------------------

template<typename MODEL> class DRIPCGMinimizer : public DRMinimizer<MODEL> {
  typedef BMatrix<MODEL>             Bmat_;
  typedef CostFunction<MODEL>        CostFct_;
  typedef ControlIncrement<MODEL>    CtrlInc_;
  typedef HtRinvHMatrix<MODEL>       HtRinvH_;

 public:
  const std::string classname() const override {return "DRIPCGMinimizer";}
  DRIPCGMinimizer(const eckit::Configuration &, const CostFct_ &);
  ~DRIPCGMinimizer() {}

 private:
  double solve(CtrlInc_ &, CtrlInc_ &, CtrlInc_ &, const Bmat_ &, const HtRinvH_ &,
               const double, const double, const int, const double) override;

  QNewtonLMP<CtrlInc_, Bmat_> lmp_;
};

// =============================================================================

template<typename MODEL>
DRIPCGMinimizer<MODEL>::DRIPCGMinimizer(const eckit::Configuration & conf, const CostFct_ & J)
  : DRMinimizer<MODEL>(J), lmp_(conf)
{}

// -----------------------------------------------------------------------------

template<typename MODEL>
double DRIPCGMinimizer<MODEL>::solve(CtrlInc_ & xx, CtrlInc_ & xh, CtrlInc_ & rr,
                                    const Bmat_ & B, const HtRinvH_ & HtRinvH,
                                    const double costJ0Jb, const double costJ0JoJc, 
                                    const int maxiter, const double tolerance) {
  CtrlInc_ ap(xh);
  CtrlInc_ pp(xh);
  CtrlInc_ ph(xh);
  CtrlInc_ ss(xh);
  CtrlInc_ sh(xh);
  CtrlInc_ dr(xh);
  CtrlInc_ ww(xh);

  std::vector<CtrlInc_> vvecs;  // for re-orthogonalization
  std::vector<CtrlInc_> zvecs;  // for re-orthogonalization
  std::vector<double> scals;  // for re-orthogonalization

  lmp_.multiply(rr, sh);
  B.multiply(sh, ss);

  double dotRr0  = dot_product(rr, rr);
  double dotSr0  = dot_product(rr, ss);
  double normReduction = 1.0;
  double rdots = dotSr0;
  double rdots_old = dotSr0;

  vvecs.push_back(rr);
  zvecs.push_back(ss);
  scals.push_back(1.0/dotSr0);

  Log::info() << std::endl;
  for (int jiter = 0; jiter < maxiter; ++jiter) {
    Log::info() << " DRIPCG Starting Iteration " << jiter+1 << std::endl;

    if (jiter == 0) {
      pp = ss;
      ph = sh;
    } else {
      dr -= rr;  // dr=oldr-r
      double beta = -dot_product(ss, dr)/rdots_old;

      pp *= beta;
      pp += ss;      // p = s + beta*p

      ph *= beta;
      ph += sh;      // ph = sh + beta*ph
    }

    HtRinvH.multiply(pp, ap);
    ap += ph;

    dr = rr;

    double rho = dot_product(pp, ap);
    double alpha = rdots/rho;

    xx.axpy(alpha, pp);   // xx = xx + alpha*pp
    xh.axpy(alpha, ph);   // xh = xh + alpha*ph
    rr.axpy(-alpha, ap);  // rr = rr - alpha*ap

    // Re-orthogonalization
    for (int jj = 0; jj < jiter; ++jj) {
      double proj = scals[jj] * dot_product(rr, zvecs[jj]);
      rr.axpy(-proj, vvecs[jj]);
    }

    lmp_.multiply(rr, sh);
    B.multiply(sh, ss);

    rdots_old = rdots;
    rdots = dot_product(rr, ss);
    normReduction = sqrt(dot_product(rr, rr)/dotRr0);

    Log::info() << "DRIPCG end of iteration " << jiter+1 << ". Norm reduction= "
                << util::full_precision(normReduction) << std::endl << std::endl;

    // Save the pairs for preconditioning
    lmp_.push(pp, ph, ap, rho);

    if (normReduction < tolerance) {
      Log::info() << "DRIPCG: Achieved required reduction in residual norm." << std::endl;
      break;
    }

    vvecs.push_back(rr);
    zvecs.push_back(ss);
    scals.push_back(1.0/rdots);
  }

// Generate the (second-level) Limited Memory Preconditioner
  lmp_.update(B);

  return normReduction;
}

// -----------------------------------------------------------------------------

}  // namespace oops

#endif  // OOPS_ASSIMILATION_DRIPCGMINIMIZER_H_
