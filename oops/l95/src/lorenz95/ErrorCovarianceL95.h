/*
 * (C) Copyright 2009-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef LORENZ95_ERRORCOVARIANCEL95_H_
#define LORENZ95_ERRORCOVARIANCEL95_H_

#include <ostream>
#include <string>
#include <vector>
#include <boost/noncopyable.hpp>

#include "eckit/config/Configuration.h"
#include "util/DateTime.h"
#include "util/ObjectCounter.h"
#include "util/Printable.h"

// Forward declarations
namespace lorenz95 {
  class IncrementL95;
  class StateL95;
  class NoVariables;
  class Resolution;

/// Background error covariance matrix for Lorenz 95 model.
/*!
 *  Gaussian background error covariance matrix for Lorenz 95 model.
 */

// -----------------------------------------------------------------------------
class ErrorCovarianceL95 : public util::Printable,
                           private boost::noncopyable,
                           private util::ObjectCounter<ErrorCovarianceL95> {
 public:
  static const std::string classname() {return "lorenz95::ErrorCovarianceL95";}

  ErrorCovarianceL95(const Resolution &, const NoVariables &,
                     const eckit::Configuration &, const StateL95 &);
  ~ErrorCovarianceL95();

  void linearize(const StateL95 &, const Resolution &);
  void multiply(const IncrementL95 &, IncrementL95 &) const;
  void inverseMultiply(const IncrementL95 &, IncrementL95 &) const;
  void randomize(IncrementL95 &) const;

 private:
  void print(std::ostream &) const;
  const util::DateTime time_;
  const double sigmab_;
  const double rscale_;
  unsigned int size_;
  std::vector<double> bcoefs_;
};
// -----------------------------------------------------------------------------
}  // namespace lorenz95

#endif  // LORENZ95_ERRORCOVARIANCEL95_H_
