/*
 * (C) Copyright 2009-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef OOPS_ASSIMILATION_PRIMALMINIMIZER_H_
#define OOPS_ASSIMILATION_PRIMALMINIMIZER_H_

#include <string>

#include "util/Logger.h"
#include "oops/assimilation/BMatrix.h"
#include "oops/assimilation/ControlIncrement.h"
#include "oops/assimilation/ControlVariable.h"
#include "oops/assimilation/CostFunction.h"
#include "oops/assimilation/HtRinvHMatrix.h"
#include "oops/assimilation/Minimizer.h"
#include "eckit/config/Configuration.h"

namespace oops {

/// Primal Minimizer
/*!
 * PrimalMinimizer is the base class for all minimizers that minimize the
 * variational data assimilation cost function in primal (model) space.
 */

// -----------------------------------------------------------------------------

template<typename MODEL> class PrimalMinimizer : public Minimizer<MODEL> {
  typedef CostFunction<MODEL>        CostFct_;
  typedef ControlIncrement<MODEL>    CtrlInc_;
  typedef ControlVariable<MODEL>     CtrlVar_;
  typedef BMatrix<MODEL>             Bmat_;
  typedef HessianMatrix<MODEL>       Hessian_;

 public:
  explicit PrimalMinimizer(const CostFct_ & J): J_(J) {}
  ~PrimalMinimizer() {}
  CtrlInc_ * minimize(const eckit::Configuration &) override;
  virtual const std::string classname() const override =0;

 private:
  virtual double solve(CtrlInc_ &, const CtrlInc_ &,
                       const Hessian_ &, const Bmat_ &,
                       const int, const double) =0;

  const CostFct_ & J_;
};

// =============================================================================

template<typename MODEL>
ControlIncrement<MODEL> * PrimalMinimizer<MODEL>::minimize(const eckit::Configuration & config) {
  int ninner = config.getInt("ninner");
  double gnreduc = config.getDouble("gradient_norm_reduction");

  Log::info() << classname() << ": max iter = " << ninner
             << ", requested norm reduction = " << gnreduc << std::endl;

// Define the matrices
  Hessian_ hessian(J_);
  Bmat_ B(J_);

// Compute RHS
  CtrlInc_ rhs(J_.jb());
  J_.computeGradientFG(rhs);
  J_.jb().addGradientFG(rhs);
  rhs *= -1.0;
  Log::info() << classname() << " rhs" << rhs << std::endl;

// Define minimisation starting point
  CtrlInc_ * dx = new CtrlInc_(J_.jb());

// Solve the linear system
  double reduc = this->solve(*dx, rhs, hessian, B, ninner, gnreduc);

  Log::test() << classname() << ": reduction in residual norm = " << reduc << std::endl;
  Log::info() << classname() << " output" << *dx << std::endl;

  return dx;
}

// -----------------------------------------------------------------------------

}  // namespace oops

#endif  // OOPS_ASSIMILATION_PRIMALMINIMIZER_H_
