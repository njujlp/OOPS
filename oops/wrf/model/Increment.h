/*
 * (C) Copyright 2017 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef WRF_MODEL_WRFINCREMENT_H_
#define WRF_MODEL_WRFINCREMENT_H_

#include <ostream>
#include <string>

#include <boost/scoped_ptr.hpp>
#include <boost/shared_ptr.hpp>

#include "model/Fields.h"
#include "model/Geometry.h"
#include "oops/base/GeneralizedDepartures.h"
#include "util/DateTime.h"
#include "util/Duration.h"
#include "util/ObjectCounter.h"
#include "util/Printable.h"
#include "util/dot_product.h"

namespace eckit {
  class Configuration;
}

namespace oops {
  class UnstructuredGrid;
}

namespace wrf {
  class State;
  class Variables;

/// Increment Class: Difference between two states
/*!
 *  Some fields that are present in a State may not be present in
 *  an Increment. The Increment contains everything that is needed by
 *  the tangent-linear and adjoint models.
 */

// -----------------------------------------------------------------------------

class Increment : public oops::GeneralizedDepartures,
                    public util::Printable,
                    private util::ObjectCounter<Increment> {
 public:
  static const std::string classname() {return "wrf::Increment";}

/// Constructor, destructor
  Increment(const Geometry &, const Variables &, const util::DateTime &);
  Increment(const Geometry &, const Increment &);
  Increment(const Increment &, const bool);
  Increment(const Increment &);
  virtual ~Increment();

/// Basic operators
  void diff(const State &, const State &);
  void zero();
  void zero(const util::DateTime &);
  void dirac(const eckit::Configuration &);
  Increment & operator =(const Increment &);
  Increment & operator+=(const Increment &);
  Increment & operator-=(const Increment &);
  Increment & operator*=(const double &);
  void axpy(const double &, const Increment &, const bool check = true);
  double dot_product_with(const Increment &) const;
  void schur_product_with(const Increment &);
  void random();

/// Interpolate to observation location
//  void interpolateTL(const LocQG &, GomQG &) const;
//  void interpolateAD(const LocQG &, const GomQG &);

/// Convert to/from generic unstructured grid
  void convert_to(oops::UnstructuredGrid &) const;
  void convert_from(const oops::UnstructuredGrid &);

/// I/O and diagnostics
  void read(const eckit::Configuration &);
  void write(const eckit::Configuration &) const;
  double norm() const {return fields_->norm();}
  const util::DateTime & validTime() const {return fields_->time();}
  util::DateTime & validTime() {return fields_->time();}
  void updateTime(const util::Duration & dt) {fields_->time() += dt;}

/// Access to fields
  Fields & fields() {return *fields_;}
  const Fields & fields() const {return *fields_;}

  boost::shared_ptr<const Geometry> geometry() const {
    return fields_->geometry();
  }

/// Other
  void activateModel();
  void deactivateModel();

  void accumul(const double &, const State &);

/// Data
 private:
  void print(std::ostream &) const;
  boost::scoped_ptr<Fields> fields_;
  boost::scoped_ptr<Fields> stash_;
};
// -----------------------------------------------------------------------------

}  // namespace wrf

#endif  // WRF_MODEL_WRFINCREMENT_H_