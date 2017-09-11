/*
 * (C) Copyright 2009-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef QG_MODEL_MODELBIASINCREMENT_H_
#define QG_MODEL_MODELBIASINCREMENT_H_

#include <iostream>

#include "util/Printable.h"

namespace eckit {
  class Configuration;
}

namespace qg {
  class ModelBias;
  class ModelBiasCovariance;
  class QgGeometry;

// -----------------------------------------------------------------------------

class ModelBiasIncrement : public util::Printable {
 public:
/// Constructor, destructor
  ModelBiasIncrement(const QgGeometry &, const eckit::Configuration &) {}
  ModelBiasIncrement(const ModelBiasIncrement &, const bool) {}
  ModelBiasIncrement(const ModelBiasIncrement &, const eckit::Configuration &) {}
  ~ModelBiasIncrement() {}

/// Linear algebra operators
  void diff(const ModelBias &, const ModelBias &) {}
  void zero() {}
  ModelBiasIncrement & operator=(const ModelBiasIncrement &) {return *this;}
  ModelBiasIncrement & operator+=(const ModelBiasIncrement &) {return *this;}
  ModelBiasIncrement & operator-=(const ModelBiasIncrement &) {return *this;}
  ModelBiasIncrement & operator*=(const double) {return *this;}
  void axpy(const double, const ModelBiasIncrement &) {}
  double dot_product_with(const ModelBiasIncrement &) const {return 0.0;}

/// I/O and diagnostics
  void read(const eckit::Configuration &) {}
  void write(const eckit::Configuration &) const {}
  double norm() const {return 0.0;}

 private:
  explicit ModelBiasIncrement(const ModelBiasCovariance &);
  void print(std::ostream & os) const {}
};

// -----------------------------------------------------------------------------

}  // namespace qg

#endif  // QG_MODEL_MODELBIASINCREMENT_H_
