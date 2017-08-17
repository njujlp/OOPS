/*
 * (C) Copyright 2017 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <iostream>

#include "model/Variables.h"
#include "model/Fortran.h"

// -----------------------------------------------------------------------------
namespace fv3 {
// -----------------------------------------------------------------------------

void Variables::print(std::ostream & os) const {
  int nv3d;
  int nv2d;
  int nl;
  fv3_var_info_f90(keyVar_, nv3d, nv2d, nl);
  os << "nv3d = " << nv3d <<", nv2d = " << nv2d;
  if (nl == 1) os << " with LBC";
  ASSERT(nl == 0 || nl == 1);
}

// -----------------------------------------------------------------------------

}  // namespace fv3
