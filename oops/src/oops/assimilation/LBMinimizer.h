/*
 * (C) Copyright 2009-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef OOPS_ASSIMILATION_LBMINIMIZER_H_
#define OOPS_ASSIMILATION_LBMINIMIZER_H_

#include <string>

#include <boost/scoped_ptr.hpp>

#include "util/Logger.h"
#include "oops/assimilation/BMatrix.h"
#include "oops/assimilation/BHessianMatrix.h"
#include "oops/assimilation/ControlIncrement.h"
#include "oops/assimilation/ControlVariable.h"
#include "oops/assimilation/CostFunction.h"
#include "oops/assimilation/HtRinvHMatrix.h"
#include "oops/assimilation/Minimizer.h"
#include "eckit/config/Configuration.h"

namespace oops {

/// LB (Left B-preconditioned) Minimizers
/*!
 * LBMinimizer is the base class for all minimizers that use \f$ B\f$ to
 * precondition the variational minimisation problem 
 *
 * NOTE: not suitable for weak constraint state formulation
 */

// -----------------------------------------------------------------------------

template<typename MODEL> class LBMinimizer : public Minimizer<MODEL> {
  typedef CostFunction<MODEL>        CostFct_;
  typedef ControlIncrement<MODEL>    CtrlInc_;
  typedef ControlVariable<MODEL>     CtrlVar_;
  typedef BMatrix<MODEL>             Bmat_;
  typedef BHessianMatrix<MODEL>      BHessianMatrix_;

 public:
  explicit LBMinimizer(const CostFct_ & J): J_(J), gradJb_(0) {}
  ~LBMinimizer() {}
  CtrlInc_ * minimize(const eckit::Configuration &) override;
  virtual const std::string classname() const override =0;

 private:
  virtual void solve(CtrlInc_ &, CtrlInc_ &, 
                     const BHessianMatrix_ &, const int, const double) =0;

  const CostFct_ & J_;
  boost::scoped_ptr<CtrlInc_> gradJb_;
};

// =============================================================================

template<typename MODEL>
ControlIncrement<MODEL> * LBMinimizer<MODEL>::minimize(const eckit::Configuration & config) {
  int ninner = config.getInt("ninner");
  double gnreduc = config.getDouble("gradient_norm_reduction");
  if (gradJb_ == 0) gradJb_.reset(new CtrlInc_(J_.jb()));

  Log::info() << std::endl;
  Log::info() << classname() << ": max iter = " << ninner <<
                ", requested norm reduction = " << gnreduc << std::endl;

// Define the matrices
  const Bmat_ B(J_);
  const BHessianMatrix_ BHessianMatrix(J_);

// Compute RHS (sum dx^{b}_{i} + ) B H^T R^{-1} d
  CtrlInc_ rhs(J_.jb());
  CtrlInc_ brhs(J_.jb());
  J_.computeGradientFG(rhs);
  J_.jb().multiplyB(rhs, brhs);
  J_.jb().addGradientFG(brhs, *gradJb_);

  brhs *= -1.0;
  Log::info() << classname() << " rhs" << brhs << std::endl;

// Define minimisation starting point
  // dx 
  CtrlInc_ * dx = new CtrlInc_(J_.jb());

// Solve the linear system
  this->solve(*dx, brhs, BHessianMatrix, ninner, gnreduc);

  Log::info() << classname() << " output increment:" << *dx << std::endl;

// Update gradient Jb
  *gradJb_ += *dx;

  return dx;
}

// -----------------------------------------------------------------------------

}  // namespace oops

#endif  // OOPS_ASSIMILATION_LBMINIMIZER_H_
