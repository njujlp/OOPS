/*
 * (C) Copyright 2017 UCAR
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 */

#ifndef UTIL_RANDOM_CPP_H_
#define UTIL_RANDOM_CPP_H_

namespace util {
extern "C" {
void random_cpp(const int &, double &);
}
}  // namespace util

#endif  // UTIL_RANDOM_CPP_H_
