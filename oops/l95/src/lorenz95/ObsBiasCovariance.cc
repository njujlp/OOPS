/*
 * (C) Copyright 2009-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "lorenz95/ObsBiasCovariance.h"

#include <cmath>
#include <iostream>
#include <stdlib.h>     /* srand, rand */
#include <string>

#include "util/Logger.h"
#include "lorenz95/ObsBiasCorrection.h"
#include "eckit/config/Configuration.h"

using oops::Log;


// -----------------------------------------------------------------------------
namespace lorenz95 {
// -----------------------------------------------------------------------------
ObsBiasCovariance::ObsBiasCovariance(const eckit::Configuration & conf)
  : conf_(conf), variance_(0.0), active_(false)
{
  if (conf_.has("standard_deviation")) {
    active_ = true;
    const double zz = conf_.getDouble("standard_deviation");
    variance_ = zz * zz;
    ASSERT(variance_ > 0.0);
    Log::info() << "ObsBiasCovariance variance = " << variance_ << std::endl;
  }
}
// -----------------------------------------------------------------------------
void ObsBiasCovariance::multiply(const ObsBiasCorrection & dxin,
                                 ObsBiasCorrection & dxout) const {
  if (active_) {
    dxout = dxin;
    dxout *= variance_;
  } else {
    dxout.zero();
  }
}
// -----------------------------------------------------------------------------
void ObsBiasCovariance::inverseMultiply(const ObsBiasCorrection & dxin,
                                        ObsBiasCorrection & dxout) const {
  if (active_) {
    dxout = dxin;
    dxout *= 1.0 / variance_;
  } else {
    dxout.zero();
  }
}
// -----------------------------------------------------------------------------
void ObsBiasCovariance::randomize(ObsBiasCorrection & dx) const {
  if (active_) {
    double zz = static_cast<double>(std::rand()) / RAND_MAX;
    dx.value() = zz * std::sqrt(variance_);
  } else {
    dx.zero();
  }
}
// -----------------------------------------------------------------------------
void ObsBiasCovariance::print(std::ostream & os) const {
  os << "ObsBiasCovariance::print not implemented";
}
// -----------------------------------------------------------------------------
}  // namespace lorenz95
