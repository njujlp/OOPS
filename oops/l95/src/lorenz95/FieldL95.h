/*
 * (C) Copyright 2009-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef LORENZ95_FIELDL95_H_
#define LORENZ95_FIELDL95_H_

#include <ostream>
#include <string>
#include <vector>

#include "util/Printable.h"

// Forward declarations
namespace eckit {
  class Configuration;
}

namespace oops {
  class UnstructuredGrid;
}

namespace lorenz95 {
  class LocsL95;
  class GomL95;
  class Resolution;

// -----------------------------------------------------------------------------
/// Class to represent fields for the L95 model
class FieldL95 : public util::Printable {
 public:
  static const std::string classname() {return "lorenz95::FieldL95";}

/// Constructors and basic operators
  explicit FieldL95(const Resolution &);
  FieldL95(const FieldL95 &, const Resolution &);
  explicit FieldL95(const FieldL95 &, const bool copy = true);
  ~FieldL95() {}

/// Linear algebra
  void zero();
  void dirac(const eckit::Configuration &);
  FieldL95 & operator=(const FieldL95 &);
  FieldL95 & operator+=(const FieldL95 &);
  FieldL95 & operator-=(const FieldL95 &);
  FieldL95 & operator*=(const double &);
  void diff(const FieldL95 &, const FieldL95 &);
  void axpy(const double &, const FieldL95 &);
  double dot_product_with(const FieldL95 &) const;
  void schur(const FieldL95 &);
  void random();

/// Utilities
  void read(std::ifstream &);
  void write(std::ofstream &) const;
  double rms() const;

/// Set and get
  const int & resol() const {return resol_;}
  double & operator[](const int ii) {return x_[ii];}
  const double & operator[](const int ii) const {return x_[ii];}
  std::vector<double> & asVector() {return x_;}
  const std::vector<double> & asVector() const {return x_;}

/// Interpolate to given location
  void interp(const LocsL95 &, GomL95 &) const;
  void interpAD(const LocsL95 &, const GomL95 &);

/// Convert to/from generic unstructured grid
  void convert_to(oops::UnstructuredGrid &) const;
  void convert_from(const oops::UnstructuredGrid &);

 private:
  void print(std::ostream &) const;
  const int resol_;
  std::vector<double> x_;
};
// -----------------------------------------------------------------------------

}  // namespace lorenz95

#endif  // LORENZ95_FIELDL95_H_
