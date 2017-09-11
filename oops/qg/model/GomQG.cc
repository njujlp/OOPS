/*
 * (C) Copyright 2009-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "model/GomQG.h"

#include "util/Logger.h"
#include "model/ObsSpaceQG.h"
#include "model/QgObservation.h"
#include "model/LinearObsOp.h"
#include "model/QgFortran.h"
#include "model/VarQG.h"

namespace qg {
  class QgGeometry;

// -----------------------------------------------------------------------------
GomQG::GomQG(const ObsSpaceQG & obsdb, const VarQG & var,
             const util::DateTime & t1, const util::DateTime & t2,
             const QgGeometry &) {
  const util::DateTime * p1 = &t1;
  const util::DateTime * p2 = &t2;
  qg_obsdb_getgom_f90(obsdb.toFortran(), obsdb.obsname().size(), obsdb.obsname().c_str(),
                      var.toFortran(), &p1, &p2, keyGom_);
}
// -----------------------------------------------------------------------------
GomQG::~GomQG() {
  qg_gom_delete_f90(keyGom_);
}
// -----------------------------------------------------------------------------
// GomQG & GomQG::operator= (const GomQG & rhs) {
//   const F90ovec * f90rhs = rhs.f90gom_;
//   qg_gom_assign_f90(keyGom_, &f90rhs);
//   return *this;
// }
// // -----------------------------------------------------------------------------
// GomQG & GomQG::operator*= (const double & zz) {
//   qg_gom_mul_scal_f90(keyGom_, zz);
//   return *this;
// }
// // -----------------------------------------------------------------------------
// GomQG & GomQG::operator+= (const GomQG & rhs) {
//   const F90ovec * f90rhs = rhs.f90gom_;
//   qg_gom_add_f90(keyGom_, &f90rhs);
//   return *this;
// }
// // -----------------------------------------------------------------------------
// GomQG & GomQG::operator-= (const GomQG & rhs) {
//   const F90ovec * f90rhs = rhs.f90gom_;
//   qg_gom_sub_f90(keyGom_, &f90rhs);
//   return *this;
// }
// // -----------------------------------------------------------------------------
// GomQG & GomQG::operator*= (const GomQG & rhs) {
//   const F90ovec * f90rhs = rhs.f90gom_;
//   qg_gom_mul_f90(keyGom_, &f90rhs);
//   return *this;
// }
// // -----------------------------------------------------------------------------
// GomQG & GomQG::operator/= (const GomQG & rhs) {
//   const F90ovec * f90rhs = rhs.f90gom_;
//   qg_gom_div_f90(keyGom_, &f90rhs);
//   return *this;
// }
// -----------------------------------------------------------------------------
void GomQG::zero() {
  qg_gom_zero_f90(keyGom_);
}
// -----------------------------------------------------------------------------
// void GomQG::random() {
//   qg_gom_random_f90(keyGom_);
// }
// -----------------------------------------------------------------------------
double GomQG::dot_product_with(const GomQG & other) const {
  double zz;
  qg_gom_dotprod_f90(keyGom_, other.toFortran(), zz);
  return zz;
}
// -----------------------------------------------------------------------------
void GomQG::print(std::ostream & os) const {
  int nn;
  double zmin, zmax, zavg;
  qg_gom_minmaxavg_f90(keyGom_, nn, zmin, zmax, zavg);
  os << " nobs= " << nn << " Min=" << zmin << ", Max=" << zmax << ", RMS=" << zavg;
}
// -----------------------------------------------------------------------------
}  // namespace qg
