/*
 * (C) Copyright 2009-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef QG_MODEL_GOMQG_H_
#define QG_MODEL_GOMQG_H_

#include <ostream>
#include <string>

#include "model/QgFortran.h"
#include "util/DateTime.h"
#include "util/ObjectCounter.h"
#include "util/Printable.h"

namespace qg {
  class ObsSpaceQG;
  class QgGeometry;
  class VarQG;

/// GomQG class to handle local model values for QG model.

class GomQG : public util::Printable,
              private util::ObjectCounter<GomQG> {
 public:
  static const std::string classname() {return "qg::GomQG";}

  GomQG(const ObsSpaceQG &, const VarQG &,
        const util::DateTime &, const util::DateTime &, const QgGeometry &);

  explicit GomQG(): keyGom_(0) {}
  explicit GomQG(int & fgom): keyGom_(fgom) {}

  ~GomQG();

  void zero();
  double dot_product_with(const GomQG & other) const;

  int & toFortran() {return keyGom_;}
  const int & toFortran() const {return keyGom_;}

 private:
  void print(std::ostream &) const;
  int keyGom_;
};

}  // namespace qg

#endif  // QG_MODEL_GOMQG_H_
