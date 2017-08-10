/*
 * (C) Copyright 2017 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef WRF_MODEL_WRFTRAITS_H_
#define WRF_MODEL_WRFTRAITS_H_

#include <string>

#include "model/Geometry.h"
#include "model/Increment.h"
#include "model/State.h"
#include "model/Variables.h"

namespace wrf {

struct Traits {
  static std::string name() {return "WRF";}

  typedef wrf::Geometry            Geometry;
  typedef wrf::Variables           Variables;
  typedef wrf::State               State;
  typedef wrf::Increment           Increment;
};

}  // namespace wrf

#endif  // WRF_MODEL_WRFTRAITS_H_
