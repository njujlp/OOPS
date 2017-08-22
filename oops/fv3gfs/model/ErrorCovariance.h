/*
 * (C) Copyright 2017 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef FV3GFS_MODEL_FV3GFSERRORCOVARIANCE_H_
#define FV3GFS_MODEL_FV3GFSERRORCOVARIANCE_H_

#include <ostream>
#include <string>
#include <boost/noncopyable.hpp>
#include <boost/scoped_ptr.hpp>

#include "model/Fortran.h"
#include "model/Geometry.h"
#include "eckit/config/Configuration.h"
#include "util/DateTime.h"
#include "util/ObjectCounter.h"
#include "util/Printable.h"

// Forward declarations
namespace fv3gfs {
  class Increment;
  class State;
  class Variables;

// -----------------------------------------------------------------------------
/// Background error covariance matrix for FV3-GFS model.

class ErrorCovariance : public util::Printable,
                        private boost::noncopyable,
                        private util::ObjectCounter<ErrorCovariance> {
 public:
  static const std::string classname() {return "fv3gfs::ErrorCovariance";}

  ErrorCovariance(const Geometry &, const Variables &, const eckit::Configuration &, const State &) {}
  ~ErrorCovariance() {}

  void linearize(const State &, const Geometry &) {}
  void multiply(const Increment &, Increment &) const {}
  void inverseMultiply(const Increment &, Increment &) const {}
  void randomize(Increment &) const {}

 private:
  void print(std::ostream &) const {}
  int keyFtnConfig_;
  boost::scoped_ptr<const Geometry> geom_;
  util::DateTime time_;
};
// -----------------------------------------------------------------------------

}  // namespace fv3gfs

#endif  // FV3GFS_MODEL_FV3GFSERRORCOVARIANCE_H_
