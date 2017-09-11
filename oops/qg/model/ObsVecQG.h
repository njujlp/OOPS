/*
 * (C) Copyright 2009-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef QG_MODEL_OBSVECQG_H_
#define QG_MODEL_OBSVECQG_H_

#include <ostream>
#include <string>

#include "model/QgFortran.h"
#include "util/ObjectCounter.h"
#include "util/Printable.h"

namespace qg {
  class ObsSpaceQG;

// -----------------------------------------------------------------------------
/// ObsVecQG class to handle vectors in observation space for QG model.

class ObsVecQG : public util::Printable,
                 private util::ObjectCounter<ObsVecQG> {
 public:
  static const std::string classname() {return "qg::ObsVecQG";}

  explicit ObsVecQG(const ObsSpaceQG &);
  ObsVecQG(const ObsVecQG &, const bool copy = true);
  ~ObsVecQG();

  ObsVecQG & operator = (const ObsVecQG &);
  ObsVecQG & operator*= (const double &);
  ObsVecQG & operator+= (const ObsVecQG &);
  ObsVecQG & operator-= (const ObsVecQG &);
  ObsVecQG & operator*= (const ObsVecQG &);
  ObsVecQG & operator/= (const ObsVecQG &);

  void zero();
  void axpy(const double &, const ObsVecQG &);
  void invert();
  void random();
  double dot_product_with(const ObsVecQG &) const;
  double rms() const;

  unsigned int size() const;

  int & toFortran() {return keyOvec_;}
  const int & toFortran() const {return keyOvec_;}

// I/O
  void read(const std::string &);
  void save(const std::string &) const;

 private:
  void print(std::ostream &) const;

  const ObsSpaceQG & obsdb_;
  int keyOvec_;
};
// -----------------------------------------------------------------------------

}  // namespace qg

#endif  // QG_MODEL_OBSVECQG_H_
