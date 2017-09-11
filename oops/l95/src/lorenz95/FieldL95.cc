/*
 * (C) Copyright 2009-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "lorenz95/FieldL95.h"

#include <cmath>
#include <limits>
#include <fstream>
#include <random>
#include <string>

#include "lorenz95/GomL95.h"
#include "lorenz95/LocsL95.h"
#include "lorenz95/Resolution.h"
#include "oops/generic/UnstructuredGrid.h"
#include "util/abor1_cpp.h"

// -----------------------------------------------------------------------------
namespace lorenz95 {
// -----------------------------------------------------------------------------
FieldL95::FieldL95(const Resolution & resol)
  : resol_(resol.npoints()), x_(resol_)
{
  ASSERT(resol_ > 0);
  for (int jj = 0; jj < resol_; ++jj) x_[jj] = 0.0;
}
// -----------------------------------------------------------------------------
FieldL95::FieldL95(const FieldL95 & other, const Resolution & resol)
  : resol_(resol.npoints()), x_(resol_)
{
  ASSERT(resol_ > 0);
  ASSERT(other.resol_ == resol_);
  for (int jj = 0; jj < resol_; ++jj) x_[jj] = other.x_[jj];
}
// -----------------------------------------------------------------------------
FieldL95::FieldL95(const FieldL95 & other, const bool copy)
  : resol_(other.resol_), x_(resol_)
{
  ASSERT(resol_ > 0);
  if (copy) {
    for (int jj = 0; jj < resol_; ++jj) x_[jj] = other.x_[jj];
  } else {
    for (int jj = 0; jj < resol_; ++jj) x_[jj] = 0.0;
  }
}
// -----------------------------------------------------------------------------
void FieldL95::zero() {
  for (int jj = 0; jj < resol_; ++jj) x_[jj] = 0.0;
}
// -----------------------------------------------------------------------------
void FieldL95::dirac(const eckit::Configuration & config) {
// Get Diracs position
  std::vector<int> ixdir(config.getIntVector("ixdir"));

// Check
  ASSERT(ixdir.size() > 0);
  for (int jj = 0; jj < ixdir.size(); ++jj) {
     ASSERT(ixdir[jj] < resol_);
  }

// Setup Dirac
  for (int jj = 0; jj < resol_; ++jj) x_[jj] = 0.0;
  for (int jj = 0; jj < ixdir.size(); ++jj) x_[ixdir[jj]] = 1.0;
}
// -----------------------------------------------------------------------------
FieldL95 & FieldL95::operator=(const FieldL95 & rhs) {
  ASSERT(rhs.resol_ == resol_);
  for (int jj = 0; jj < resol_; ++jj) x_[jj] = rhs.x_[jj];
  return *this;
}
// -----------------------------------------------------------------------------
FieldL95 & FieldL95::operator+= (const FieldL95 & rhs) {
  ASSERT(rhs.resol_ == resol_);
  for (int jj = 0; jj < resol_; ++jj) x_[jj] += rhs.x_[jj];
  return *this;
}
// -----------------------------------------------------------------------------
FieldL95 & FieldL95::operator-= (const FieldL95 & rhs) {
  ASSERT(rhs.resol_ == resol_);
  for (int jj = 0; jj < resol_; ++jj) x_[jj] -= rhs.x_[jj];
  return *this;
}
// -----------------------------------------------------------------------------
FieldL95 & FieldL95::operator*= (const double & fact) {
  for (int jj = 0; jj < resol_; ++jj) x_[jj] *= fact;
  return *this;
}
// -----------------------------------------------------------------------------
void FieldL95::diff(const FieldL95 & x1, const FieldL95 & x2) {
  ASSERT(x1.resol_ == resol_);
  ASSERT(x2.resol_ == resol_);
  for (int jj = 0; jj < resol_; ++jj) {
    x_[jj] = x1.x_[jj] - x2.x_[jj];
  }
}
// -----------------------------------------------------------------------------
void FieldL95::axpy(const double & zz, const FieldL95 & rhs) {
  ASSERT(rhs.resol_ == resol_);
  for (int jj = 0; jj < resol_; ++jj) x_[jj] += zz * rhs.x_[jj];
}
// -----------------------------------------------------------------------------
double FieldL95::dot_product_with(const FieldL95 & other) const {
  ASSERT(other.resol_ == resol_);
  double zz = 0.0;
  for (int jj = 0; jj < resol_; ++jj) zz += x_[jj] * other.x_[jj];
  return zz;
}
// -----------------------------------------------------------------------------
void FieldL95::schur(const FieldL95 & rhs) {
  ASSERT(rhs.resol_ == resol_);
  for (int jj = 0; jj < resol_; ++jj) x_[jj] *= rhs.x_[jj];
}
// -----------------------------------------------------------------------------
void FieldL95::random() {
  const double stdev = 1.0;
  const int seed = 1;
  static std::default_random_engine generator(seed);
  static std::normal_distribution<double> distribution(0.0, stdev);
  for (int jj = 0; jj < resol_; ++jj) x_[jj] = distribution(generator);
}
// -----------------------------------------------------------------------------
void FieldL95::interp(const LocsL95 & locs, GomL95 & gom) const {
  const double dres = static_cast<double>(resol_);
  for (int jobs = 0; jobs < locs.nobs(); ++jobs) {
    int ii = round(locs[jobs] * dres);
    ASSERT(ii >= 0 && ii <= resol_);
    if (ii == resol_) ii = 0;
    gom[gom.current()+jobs] = x_[ii];
  }
  gom.current() += locs.nobs();
}
// -----------------------------------------------------------------------------
void FieldL95::interpAD(const LocsL95 & locs, const GomL95 & gom) {
  const double dres = static_cast<double>(resol_);
  if (gom.current() == 0) gom.current() = gom.nobs();
  gom.current() -= locs.nobs();
  for (int jobs = 0; jobs < locs.nobs(); ++jobs) {
    int ii = round(locs[jobs] * dres);
    ASSERT(ii >= 0 && ii <= resol_);
    if (ii == resol_) ii = 0;
    x_[ii] += gom[gom.current()+jobs];
  }
}
// -----------------------------------------------------------------------------
void FieldL95::convert_to(oops::UnstructuredGrid & ug) const {
  ABORT("FieldL95 conversion to unstructured grid not implemented.");
}
// -----------------------------------------------------------------------------
void FieldL95::convert_from(const oops::UnstructuredGrid & ug) {
  ABORT("FieldL95 conversion from unstructured grid not implemented.");
}
// -----------------------------------------------------------------------------
void FieldL95::read(std::ifstream & fin) {
  for (int jj = 0; jj < resol_; ++jj) fin >> x_[jj];
}
// -----------------------------------------------------------------------------
void FieldL95::write(std::ofstream & fout) const {
  fout.precision(std::numeric_limits<double>::digits10);
  for (int jj = 0; jj < resol_; ++jj) fout << x_[jj] << " ";
}
// -----------------------------------------------------------------------------
double FieldL95::rms() const {
  double zz = 0.0;
  for (int jj = 0; jj < resol_; ++jj) zz += x_[jj] * x_[jj];
  zz = sqrt(zz/resol_);
  return zz;
}
// -----------------------------------------------------------------------------
void FieldL95::print(std::ostream & os) const {
  double zmin = x_[0];
  double zmax = x_[0];
  double zavg = 0.0;
  for (int jj = 0; jj < resol_; ++jj) {
    if (x_[jj] < zmin) zmin = x_[jj];
    if (x_[jj] > zmax) zmax = x_[jj];
    zavg += x_[jj];
  }
  zavg /= resol_;
  os << " Min=" << zmin << ", Max=" << zmax << ", Average=" << zavg;
}
// -----------------------------------------------------------------------------

}  // namespace lorenz95
