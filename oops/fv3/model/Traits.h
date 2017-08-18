/*
 * (C) Copyright 2017 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef FV3_MODEL_FV3TRAITS_H_
#define FV3_MODEL_FV3TRAITS_H_

#include <string>

#include "model/ErrorCovariance.h"
#include "model/Geometry.h"
#include "model/Increment.h"
#include "model/State.h"
#include "model/Variables.h"

namespace fv3 {

struct Traits {
  static std::string name() {return "FV3";}
  static std::string nameCovar() {return "FV3Error";}

  typedef fv3::Geometry            Geometry;
  typedef fv3::Variables           Variables;
  typedef fv3::State               State;
  typedef fv3::Increment           Increment;
  typedef fv3::ErrorCovariance     Covariance;
};

}  // namespace fv3

#endif  // FV3_MODEL_FV3TRAITS_H_
