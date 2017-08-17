
#ifndef FV3_MODEL_FV3GEOMETRY_H_
#define FV3_MODEL_FV3GEOMETRY_H_

#include <ostream>
#include <string>

#include "model/Fortran.h"
#include "util/ObjectCounter.h"
#include "util/Printable.h"

namespace eckit {
  class Configuration;
}

namespace fv3 {

// -----------------------------------------------------------------------------
/// Geometry handles geometry for FV3 model.

class Geometry : public util::Printable,
                     private util::ObjectCounter<Geometry> {
 public:
  static const std::string classname() {return "fv3::Geometry";}

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

}  // namespace fv3

#endif  // FV3_MODEL_FV3GEOMETRY_H_
