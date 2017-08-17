
#include "util/Logger.h"
#include "model/Geometry.h"
#include "model/Fortran.h"
#include "eckit/config/Configuration.h"

// -----------------------------------------------------------------------------
namespace fv3 {
// -----------------------------------------------------------------------------
Geometry::Geometry(const eckit::Configuration & conf) {
  const eckit::Configuration * configc = &conf;
  fv3_geo_setup_f90(keyGeom_, &configc);
}
// -----------------------------------------------------------------------------
Geometry::Geometry(const Geometry & other) {
  const int key_geo = other.keyGeom_;
  fv3_geo_clone_f90(key_geo, keyGeom_);
}
// -----------------------------------------------------------------------------
Geometry::~Geometry() {
  fv3_geo_delete_f90(keyGeom_);
}
// -----------------------------------------------------------------------------
void Geometry::print(std::ostream & os) const {
  int pe;
  int nx;
  int ny;
  int ibeg;
  int jbeg;
  fv3_geo_info_f90(keyGeom_, pe, nx, ny, ibeg, jbeg);
  os << "pe = " << pe << ",nx = " << nx << ", ny = " << ny << 
  ", ibeg = " <<  ibeg << ", jbeg = " << jbeg ;
}
// -----------------------------------------------------------------------------
}  // namespace fv3
