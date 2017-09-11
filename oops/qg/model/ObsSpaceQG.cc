/*
 * (C) Copyright 2009-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "model/ObsSpaceQG.h"

#include <map>
#include <string>

#include "eckit/config/Configuration.h"

#include "util/abor1_cpp.h"
#include "util/Logger.h"

#include "model/ObsVecQG.h"

using oops::Log;

namespace qg {
// -----------------------------------------------------------------------------
std::map < std::string, int > ObsSpaceQG::theObsFileCount_;
// -----------------------------------------------------------------------------

ObsSpaceQG::ObsSpaceQG(const eckit::Configuration & config,
                       const util::DateTime & bgn, const util::DateTime & end)
  : winbgn_(bgn), winend_(end)
{
  static std::map < std::string, ObsHelpQG * > theObsFileRegister_;
  typedef std::map< std::string, ObsHelpQG * >::iterator otiter;

  std::string ofin("-");
  if (config.has("ObsData.ObsDataIn")) {
    ofin = config.getString("ObsData.ObsDataIn.obsfile");
  }
  std::string ofout("-");
  if (config.has("ObsData.ObsDataOut")) {
    ofout = config.getString("ObsData.ObsDataOut.obsfile");
  }
  Log::trace() << "ObsSpaceQG: Obs files are: " << ofin << " and " << ofout << std::endl;
  ref_ = ofin + ofout;
  if (ref_ == "--") {
    ABORT("Underspecified observation files.");
  }

  otiter it = theObsFileRegister_.find(ref_);
  if (it == theObsFileRegister_.end()) {
    // Open new file
    Log::trace() << "ObsSpaceQG::getHelper: " << "Opening " << ref_ << std::endl;
    helper_ = new ObsHelpQG(config);
    theObsFileCount_[ref_]=1;
    theObsFileRegister_[ref_]=helper_;
    Log::trace() << "ObsSpaceQG created, count=" << theObsFileCount_[ref_] << std::endl;
    ASSERT(theObsFileCount_[ref_] == 1);
  } else {
    // File already open
    Log::trace() << "ObsSpaceQG::getHelper: " << ref_ << " already opened." << std::endl;
    helper_ = it->second;
    theObsFileCount_[ref_]+=1;
    Log::trace() << "ObsSpaceQG count=" << theObsFileCount_[ref_] << std::endl;
    ASSERT(theObsFileCount_[ref_] > 1);
  }

  obsname_ = config.getString("ObsType");
  nobs_ = helper_->nobs(obsname_);

  // Very UGLY!!!
  nout_ = 0;
  if (obsname_ == "Stream") nout_ = 1;
  if (obsname_ == "WSpeed") nout_ = 1;
  if (obsname_ == "Wind") nout_ = 2;
  ASSERT(nout_ > 0);
  nvin_ = 0;
  if (obsname_ == "Stream") nvin_ = 1;
  if (obsname_ == "WSpeed") nvin_ = 2;
  if (obsname_ == "Wind") nvin_ = 2;
  ASSERT(nvin_ > 0);
}

// -----------------------------------------------------------------------------

void ObsSpaceQG::printJo(const ObsVecQG & dy, const ObsVecQG & grad) {
  Log::info() << "ObsSpaceQG::printJo not implemented" << std::endl;
} 

// -----------------------------------------------------------------------------

ObsSpaceQG::~ObsSpaceQG() {
  ASSERT(theObsFileCount_[ref_] > 0);
  theObsFileCount_[ref_]-=1;
  Log::trace() << "ObsSpaceQG cleared, count=" << theObsFileCount_[ref_] << std::endl;
  if (theObsFileCount_[ref_] == 0) {
    delete helper_;
  }
}

// -----------------------------------------------------------------------------

ObsSpaceQG::ObsSpaceQG(const ObsSpaceQG & other)
  : ref_(other.ref_), helper_(other.helper_),
    obsname_(other.obsname_), nobs_(other.nobs_), nvin_(other.nvin_), nout_(other.nout_)
{
  ASSERT(theObsFileCount_[ref_] > 0);
  theObsFileCount_[ref_]+=1;
  Log::trace() << "ObsSpaceQG copied, count=" << theObsFileCount_[ref_] << std::endl;
}

// -----------------------------------------------------------------------------

void ObsSpaceQG::print(std::ostream & os) const {
  os << "ObsSpaceQG::print not implemented";
}

// -----------------------------------------------------------------------------

}  // namespace qg
