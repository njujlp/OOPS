/*
 * (C) Copyright 2017 UCAR
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 */

#include "util/random_cpp.h"
#include <random>

// -----------------------------------------------------------------------------
namespace util {
// -----------------------------------------------------------------------------

void random_cpp(const int & nn, double & xx) {
  static std::default_random_engine generator(7);
  static std::normal_distribution<double> distribution(0.0, 1.0);
  xx = distribution(generator);

//  for (int jj = 0; jj < nn; ++jj) xx[jj] = distribution(generator);
}

// -----------------------------------------------------------------------------

}  // namespace util
