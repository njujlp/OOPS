/*
 * (C) Copyright 2009-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef QG_MODEL_VARQG_H_
#define QG_MODEL_VARQG_H_

#include <ostream>
#include <string>

#include "util/Logger.h"
#include "model/QgFortran.h"
#include "eckit/config/Configuration.h"
#include "util/ObjectCounter.h"
#include "util/Printable.h"

namespace qg {

// -----------------------------------------------------------------------------
/// VarQG class to handle variables for QG model.

class VarQG : public util::Printable,
              private util::ObjectCounter<VarQG> {
 public:
  static const std::string classname() {return "qg::VarQG";}

  explicit VarQG(const eckit::Configuration & config) {
    using oops::Log;
    Log::debug() << "VarQG config:" << config << std::endl;
    const eckit::Configuration * conf = &config;
    qg_var_create_f90(keyVar_, &conf);
  }
  explicit VarQG(const int keyVar): keyVar_(keyVar) {}

  ~VarQG() {qg_var_delete_f90(keyVar_);}

  VarQG(const VarQG & other) {qg_var_clone_f90(other.keyVar_, keyVar_);}

  int& toFortran() {return keyVar_;}
  const int& toFortran() const {return keyVar_;}

 private:
  void print(std::ostream &) const;
  int keyVar_;
};

// -----------------------------------------------------------------------------

}  // namespace qg

#endif  // QG_MODEL_VARQG_H_
