/*
 * (C) Copyright 2009-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef OOPS_ASSIMILATION_HTMATRIX_H_
#define OOPS_ASSIMILATION_HTMATRIX_H_

#include <boost/noncopyable.hpp>

#include "oops/assimilation/ControlIncrement.h"
#include "oops/assimilation/CostFunction.h"
#include "oops/assimilation/DualVector.h"
#include "oops/base/PostProcessorAD.h"
#include "oops/interface/Increment.h"

namespace oops {

/// The \f$ H^T \f$ matrix.
/*!
 *  The solvers represent matrices as objects that implement a "multiply"
 *  method. This class defines objects that apply a generalized \f$ H^T \f$
 *  matrix which includes \f$ H^T \f$ itself and the equivalent operators
 *  for the other terms of the cost function.
 */

template<typename MODEL> class HtMatrix : private boost::noncopyable {
  typedef Increment<MODEL>           Increment_;
  typedef CostFunction<MODEL>        CostFct_;

 public:
  explicit HtMatrix(const CostFct_ & j): j_(j) {}

  void multiply(const DualVector<MODEL> & dy, ControlIncrement<MODEL> & dx) const {
    j_.zeroAD(dx);
    PostProcessorAD<Increment_> cost;
    for (unsigned jj = 0; jj < j_.nterms(); ++jj) {
      cost.enrollProcessor(j_.jterm(jj).setupAD(dy.getv(jj), dx));
    }
    j_.runADJ(dx, cost);
  }

 private:
  CostFct_ const & j_;
};

}  // namespace oops

#endif  // OOPS_ASSIMILATION_HTMATRIX_H_
