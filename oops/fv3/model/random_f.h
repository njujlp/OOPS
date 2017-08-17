/*
 * (C) Copyright 2017 UCAR
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 */

#ifndef FV3_MODEL_RANDOM_F_H_
#define FV3_MODEL_RANDOM_F_H_

namespace fv3 {
extern "C" {
  void random_f(const int &, double *);
}
}  // namespace fv3

#endif  // FV3_MODEL_RANDOM_F_H_
