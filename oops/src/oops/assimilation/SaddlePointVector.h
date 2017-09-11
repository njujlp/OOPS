/*
 * (C) Copyright 2009-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef OOPS_ASSIMILATION_SADDLEPOINTVECTOR_H_
#define OOPS_ASSIMILATION_SADDLEPOINTVECTOR_H_

#include <boost/scoped_ptr.hpp>

#include "oops/assimilation/ControlIncrement.h"
#include "oops/assimilation/DualVector.h"
#include "util/dot_product.h"

namespace oops {

/*!
 * Control vector for the saddle point formulation.
 * The vector contains two ControlIncrements and one Departure,
 * and knows how to do basic linear algebra.
 */
/// Control vector for the saddle point formulation.

// -----------------------------------------------------------------------------
template<typename MODEL> class SaddlePointVector {
  typedef ControlIncrement<MODEL>    CtrlInc_;
  typedef DualVector<MODEL>          Multipliers_;

 public:
  SaddlePointVector(CtrlInc_ *, Multipliers_ *);
  SaddlePointVector(const SaddlePointVector &);

/// Accessor method to get the lambda_ component
  const Multipliers_ & lambda() const {return *lambda_;}
  Multipliers_ & lambda() {return *lambda_;}

/// Accessor method to set the lambda_ component
  void lambda(Multipliers_ * lambda) {lambda_.reset(lambda);}

/// Accessor method to get the dx_ component
  const CtrlInc_ & dx() const {return *dx_;}
  CtrlInc_ & dx() {return *dx_;}

/// Accessor method to set the dx_ component
  void dx(CtrlInc_ * dx) {dx_.reset(dx);}

  SaddlePointVector & operator=(const SaddlePointVector &);
  SaddlePointVector & operator+=(const SaddlePointVector &);
  SaddlePointVector & operator-=(const SaddlePointVector &);
  SaddlePointVector & operator*=(const double);
  void zero();
  void axpy(const double, const SaddlePointVector &);
  double dot_product_with(const SaddlePointVector &) const;

 private:
  boost::scoped_ptr<Multipliers_> lambda_;
  boost::scoped_ptr<CtrlInc_> dx_;
};

// =============================================================================

template<typename MODEL>
SaddlePointVector<MODEL>::SaddlePointVector(CtrlInc_ * dx,
                                            Multipliers_ * lambda)
  : lambda_(lambda), dx_(dx)
{}

// -----------------------------------------------------------------------------

template<typename MODEL>
SaddlePointVector<MODEL>::SaddlePointVector(const SaddlePointVector & other)
  : lambda_(new Multipliers_(*other.lambda_)), dx_(new CtrlInc_(*other.dx_))
{}

// -----------------------------------------------------------------------------

template<typename MODEL> SaddlePointVector<MODEL> &
        SaddlePointVector<MODEL>::operator=(const SaddlePointVector & rhs) {
  *lambda_ = *rhs.lambda_;
  *dx_     = *rhs.dx_;
  return *this;
}

// -----------------------------------------------------------------------------

template<typename MODEL> SaddlePointVector<MODEL> &
        SaddlePointVector<MODEL>::operator+=(const SaddlePointVector & rhs) {
  *lambda_ += *rhs.lambda_;
  *dx_     += *rhs.dx_;
  return *this;
}

// -----------------------------------------------------------------------------

template<typename MODEL> SaddlePointVector<MODEL> &
        SaddlePointVector<MODEL>::operator-=(const SaddlePointVector & rhs) {
  *lambda_ -= *rhs.lambda_;
  *dx_     -= *rhs.dx_;
  return *this;
}

// -----------------------------------------------------------------------------

template<typename MODEL> SaddlePointVector<MODEL> &
        SaddlePointVector<MODEL>::operator*=(const double rhs) {
  *lambda_ *= rhs;
  *dx_     *= rhs;
  return *this;
}

// -----------------------------------------------------------------------------

template<typename MODEL> void SaddlePointVector<MODEL>::zero() {
  lambda_->zero();
  dx_->zero();
}

// -----------------------------------------------------------------------------

template<typename MODEL> void SaddlePointVector<MODEL>::axpy(const double zz,
                                               const SaddlePointVector & rhs) {
  lambda_->axpy(zz, *rhs.lambda_);
  dx_->axpy(zz, *rhs.dx_);
}

// -----------------------------------------------------------------------------

template<typename MODEL> double SaddlePointVector<MODEL>::dot_product_with(
                                    const SaddlePointVector & x2) const {
return dot_product(*lambda_, *x2.lambda_)
      +dot_product(*dx_, *x2.dx_);
}

// -----------------------------------------------------------------------------

}  // namespace oops

#endif  // OOPS_ASSIMILATION_SADDLEPOINTVECTOR_H_
