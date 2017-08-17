/*
 * (C) Copyright 2017 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef FV3GFS_MODEL_FV3GFSGEOMETRY_H_
#define FV3GFS_MODEL_FV3GFSGEOMETRY_H_

#include <ostream>
#include <string>

#include "model/Fortran.h"
#include "util/ObjectCounter.h"
#include "util/Printable.h"

namespace eckit {
  class Configuration;
}

namespace fv3gfs {

// -----------------------------------------------------------------------------
/// Geometry handles geometry for FV3GFS model.

class Geometry : public util::Printable,
                     private util::ObjectCounter<Geometry> {
 public:
  static const std::string classname() {return "fv3gfs::Geometry";}

  explicit Geometry(const eckit::Configuration &);
  Geometry(const Geometry &);
  ~Geometry();

  int& toFortran() {return keyGeom_;}
  const int& toFortran() const {return keyGeom_;}

 private:
  Geometry & operator=(const Geometry &);
  void print(std::ostream &) const;
  int keyGeom_;
};
// -----------------------------------------------------------------------------

}  // namespace fv3gfs

#endif  // FV3GFS_MODEL_FV3GFSGEOMETRY_H_
