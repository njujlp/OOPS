/*
 * (C) Copyright 2017 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef FV3GFS_MODEL_FV3GFSTRAITS_H_
#define FV3GFS_MODEL_FV3GFSTRAITS_H_

#include <string>

#include "model/Geometry.h"
#include "model/Increment.h"
#include "model/State.h"
#include "model/Variables.h"

namespace fv3gfs {

struct Traits {
  static std::string name() {return "FV3GFS";}

  typedef fv3gfs::Geometry            Geometry;
  typedef fv3gfs::Variables           Variables;
  typedef fv3gfs::State               State;
  typedef fv3gfs::Increment           Increment;
};

}  // namespace fv3gfs

#endif  // FV3GFS_MODEL_FV3GFSTRAITS_H_
