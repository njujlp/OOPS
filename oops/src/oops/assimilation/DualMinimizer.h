/*
 * (C) Copyright 2009-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef OOPS_ASSIMILATION_DUALMINIMIZER_H_
#define OOPS_ASSIMILATION_DUALMINIMIZER_H_

#include <string>

#include <boost/scoped_ptr.hpp>

#include "util/Logger.h"
#include "oops/assimilation/BMatrix.h"
#include "oops/assimilation/ControlIncrement.h"
#include "oops/assimilation/ControlVariable.h"
#include "oops/assimilation/CostFunction.h"
#include "oops/assimilation/DualVector.h"
#include "oops/assimilation/HBHtMatrix.h"
#include "oops/assimilation/HMatrix.h"
#include "oops/assimilation/HtMatrix.h"
#include "oops/assimilation/Minimizer.h"
#include "oops/assimilation/RinvMatrix.h"
#include "eckit/config/Configuration.h"
#include "util/dot_product.h"

namespace oops {

/// Dual Minimizer
/*!
 * Base class for all dual (observation) space minimizers.
 */

// -----------------------------------------------------------------------------

template<typename MODEL> class DualMinimizer : public Minimizer<MODEL> {
  typedef ControlIncrement<MODEL>    CtrlInc_;
  typedef ControlVariable<MODEL>     CtrlVar_;
  typedef CostFunction<MODEL>        CostFct_;
  typedef BMatrix<MODEL>             Bmat_;
  typedef DualVector<MODEL>          Dual_;
  typedef HBHtMatrix<MODEL>          HBHt_;
  typedef RinvMatrix<MODEL>          Rinv_;

 public:
  explicit DualMinimizer(const CostFct_ & J): J_(J), gradJb_(0) {}
  ~DualMinimizer() {}
  CtrlInc_ *  minimize(const eckit::Configuration &) override;
  virtual const std::string classname() const override =0;

 private:
  virtual double solve(Dual_ &, double &, Dual_ &, const HBHt_ &, const Rinv_ &,
                       const int &, const double &, Dual_ &, const double &) =0;

  const CostFct_ & J_;
  boost::scoped_ptr<CtrlInc_> gradJb_;
};

// =============================================================================

template<typename MODEL>
ControlIncrement<MODEL> * DualMinimizer<MODEL>::minimize(const eckit::Configuration & config) {
  int ninner = config.getInt("ninner");
  double gnreduc = config.getDouble("gradient_norm_reduction");

  Log::info() << classname() << ": max iter = " << ninner
            << ", requested norm reduction = " << gnreduc << std::endl;
  if (gradJb_ == 0) gradJb_.reset(new CtrlInc_(J_.jb()));

// Define the matrices
  const Bmat_ B(J_);
  const HBHt_ HBHt(J_);
  const Rinv_ Rinv(J_);
  const HMatrix<MODEL> H(J_);
  const HtMatrix<MODEL> Ht(J_);

// Define minimisation starting point in dual space
  Dual_ vv;
  for (unsigned jj = 0; jj < J_.nterms(); ++jj) {
    vv.append(J_.jterm(jj).newDualVector());
  }
  double vvp;

// Get R^{-1} d
  Dual_ rr;
  for (unsigned jj = 0; jj < J_.nterms(); ++jj) {
    rr.append(J_.jterm(jj).newGradientFG());
  }
  rr *= -1.0;

// Update rr if initial dx in model space is not a zero vector
// rr = rr - Rinv H dx0

// Compute a = H (xb - xk)
  Dual_ dy;
  H.multiply(J_.jb().getFirstGuess(), dy);
  dy *= -1.0;

// Compute sigma = (xb - xk)^T Binv (xb - xk)
  CtrlInc_ g0(J_.jb());
  J_.jb().addGradientFG(g0, *gradJb_);

  double sigma = dot_product(J_.jb().getFirstGuess(), g0);

// Solve the linear system
  double reduc = this->solve(vv, vvp, rr, HBHt, Rinv, ninner, gnreduc, dy, sigma);

  Log::test() << classname() << ": reduction in residual norm = " << reduc << std::endl;

// Recover solution in primal space
  CtrlInc_ dh(J_.jb());
  Ht.multiply(vv, dh);
  dh.axpy(-vvp, g0);

  CtrlInc_ * dx = new CtrlInc_(J_.jb());
  B.multiply(dh, *dx);    // BHtaug vvaug

  Log::info() << classname() << ": Estimated Final Jb = " << 0.5 * dot_product(*dx, dh) << std::endl;
  Log::info() << classname() << " output" << *dx << std::endl;

// Update gradient Jb
  *gradJb_ += dh;

  return dx;
}

// -----------------------------------------------------------------------------

}  // namespace oops

#endif  // OOPS_ASSIMILATION_DUALMINIMIZER_H_
