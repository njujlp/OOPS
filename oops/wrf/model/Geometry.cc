/*
 * (C) Copyright 2017 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "util/Logger.h"
#include "model/Geometry.h"
#include "model/Fortran.h"
#include "eckit/config/Configuration.h"

// -----------------------------------------------------------------------------
namespace wrf {
// -----------------------------------------------------------------------------
Geometry::Geometry(const eckit::Configuration & conf) {
  const eckit::Configuration * configc = &conf;
  wrf_geo_setup_f90(keyGeom_, &configc);
}
// -----------------------------------------------------------------------------
Geometry::Geometry(const Geometry & other) {
  const int key_geo = other.keyGeom_;
  wrf_geo_clone_f90(key_geo, keyGeom_);
}
// -----------------------------------------------------------------------------
Geometry::~Geometry() {
  wrf_geo_delete_f90(keyGeom_);
}
// -----------------------------------------------------------------------------
void Geometry::print(std::ostream & os) const {
  int nlon;
  int nlat;
  int nlev;
  double dx;
  double dy;
// std::string gridfname(256);

// wrf_geo_info_f90(keyGeom_, nlon, nlat, nlev);
   wrf_geo_info_f90(keyGeom_, nlon, nlat, nlev, dx, dy);

  os << "nlon = " << nlon << ", nlat = " << nlat << ", nlev = " << nlev << ", dx = " << dx << ", dy = " << dy ;
}
// -----------------------------------------------------------------------------
}  // namespace wrf
