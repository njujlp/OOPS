/*
 * (C) Copyright 2009-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef QG_MODEL_OBSWINDQG_H_
#define QG_MODEL_OBSWINDQG_H_

#include <ostream>
#include <string>
#include <vector>

#include <boost/scoped_ptr.hpp>
#include <boost/shared_ptr.hpp>

#include "model/ObsSpaceQG.h"
#include "model/QgObservation.h"
#include "model/ObsWindTLAD.h"
#include "util/ObjectCounter.h"

// Forward declarations
namespace eckit {
  class Configuration;
}

namespace util {
  class DateTime;
}

namespace qg {
  class GomQG;
  class LocQG;
  class ObsBias;
  class ObsBiasIncrement;
  class ObsVecQG;

// -----------------------------------------------------------------------------
/// Wind observation for QG model.
/*!
 *  ObsWindQG for QG model inherits from ObsEquivalent.
 */

class ObsWindQG : public QgObservation,
                  private util::ObjectCounter<ObsWindQG> {
 public:
  static const std::string classname() {return "qg::ObsWindQG";}

  ObsWindQG(ObsSpaceQG &, const eckit::Configuration &);
  virtual ~ObsWindQG();

// Obs Operators
  void obsEquiv(const GomQG &, ObsVecQG &, const ObsBias &) const;

// Is there a way to put this in the TLAD class?
  LinearObsOp * getTLAD() const {return new ObsWindTLAD(obsdb_, keyOperWind_);}

// Other
  void generateObsError(const eckit::Configuration &);
  boost::shared_ptr<const VarQG> variables() const {return varin_;}

  int & toFortran() {return keyOperWind_;}
  const int & toFortran() const {return keyOperWind_;}

 private:
  void print(std::ostream &) const;
  ObsSpaceQG & obsdb_;
  const std::string obsname_;
  int keyOperWind_;
  boost::shared_ptr<const VarQG> varin_;
};
// -----------------------------------------------------------------------------

}  // namespace qg
#endif  // QG_MODEL_OBSWINDQG_H_
