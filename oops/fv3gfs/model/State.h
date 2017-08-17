/*
 * (C) Copyright 2017 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef FV3GFS_MODEL_FV3GFSSTATE_H_
#define FV3GFS_MODEL_FV3GFSSTATE_H_

#include <ostream>
#include <string>

#include <boost/scoped_ptr.hpp>

#include "model/Fields.h"
#include "util/DateTime.h"
#include "util/ObjectCounter.h"
#include "util/Printable.h"

namespace eckit {
  class Configuration;
}

namespace oops {
  class UnstructuredGrid;
}

namespace fv3gfs {
  class Geometry;
  class Increment;
  class Variables;

/// FV3GFS model state
/*!
 * A State contains everything that is needed to propagate the state
 * forward in time.
 */

// -----------------------------------------------------------------------------
class State : public util::Printable,
                private util::ObjectCounter<State> {
 public:
  static const std::string classname() {return "fv3gfs::State";}

/// Constructor, destructor
  State(const Geometry &, const Variables &, const util::DateTime &);  // Is it used?
  State(const Geometry &, const eckit::Configuration &);
  State(const Geometry &, const State &);
  State(const State &);
  virtual ~State();
  State & operator=(const State &);

/// Interpolate to observation location
//  void interpolate(const LocQG &, GomQG &) const;

/// Interpolate full fields
  void changeResolution(const State & xx);

/// Interactions with Increment
  State & operator+=(const Increment &);

/// Convert to/from generic unstructured grid
  void convert_to(oops::UnstructuredGrid &) const;
  void convert_from(const oops::UnstructuredGrid &);

/// I/O and diagnostics
  void read(const eckit::Configuration &);
  void write(const eckit::Configuration &) const;
  double norm() const {return fields_->norm();}
  const util::DateTime & validTime() const {return fields_->time();}
  util::DateTime & validTime() {return fields_->time();}

/// Access to fields
  Fields & fields() {return *fields_;}
  const Fields & fields() const {return *fields_;}

  boost::shared_ptr<const Geometry> geometry() const {
    return fields_->geometry();
  }

/// Other
  void activateModel();
  void deactivateModel();

  void zero();
  void accumul(const double &, const State &);

 private:
  void print(std::ostream &) const;
  boost::scoped_ptr<Fields> fields_;
  boost::scoped_ptr<Fields> stash_;
};
// -----------------------------------------------------------------------------

}  // namespace fv3gfs

#endif  // FV3GFS_MODEL_FV3GFSSTATE_H_
