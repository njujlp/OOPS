/*
 * (C) Copyright 2009-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "model/ObsHelpQG.h"

#include <string>

#include "util/Logger.h"
#include "model/QgObservation.h"
#include "model/QgFortran.h"
#include "eckit/config/Configuration.h"
#include "util/DateTime.h"
#include "util/Duration.h"

using oops::Log;

namespace qg {

// -----------------------------------------------------------------------------

ObsHelpQG::ObsHelpQG(const eckit::Configuration & config) {
  const eckit::Configuration * configc = &config;
  qg_obsdb_setup_f90(keyHelp_, &configc);
  Log::trace() << "ObsHelpQG constructed" << std::endl;
}

// -----------------------------------------------------------------------------

ObsHelpQG::~ObsHelpQG() {
  qg_obsdb_delete_f90(keyHelp_);
  Log::trace() << "ObsHelpQG destructed" << std::endl;
}

// -----------------------------------------------------------------------------

void ObsHelpQG::putdb(const std::string & obsname, const std::string & col, const int & keyFvec) {
  Log::trace() << "ObsHelpQG:putdb obsname = " << obsname << ", col = " << col << std::endl;
  qg_obsdb_put_f90(keyHelp_, obsname.size(), obsname.c_str(), col.size(), col.c_str(), keyFvec);
}

// -----------------------------------------------------------------------------

void ObsHelpQG::getdb(const std::string & obsname, const std::string & col, int & keyFvec) const {
  Log::trace() << "ObsHelpQG:getdb obsname = " << obsname << ", col = " << col << std::endl;
  qg_obsdb_get_f90(keyHelp_, obsname.size(), obsname.c_str(), col.size(), col.c_str(), keyFvec);
}

// -----------------------------------------------------------------------------

int ObsHelpQG::locations(const std::string & obsname,
                               const util::DateTime & t1, const util::DateTime & t2) const {
  const util::DateTime * p1 = &t1;
  const util::DateTime * p2 = &t2;
  int key_locs;
  qg_obsdb_locations_f90(keyHelp_, obsname.size(), obsname.c_str(), &p1, &p2, key_locs);
  return key_locs;
}

// -----------------------------------------------------------------------------

void ObsHelpQG::generateDistribution(const eckit::Configuration & config, const std::string & obsname,
                                     const util::DateTime & t1, const util::DateTime & t2,
                                     unsigned int & nobs) {
  const eckit::Configuration * configc = &config;
  const util::Duration first(config.getString("begin"));
  const util::DateTime start(t1 + first);
  const util::Duration freq(config.getString("obs_period"));
  int nobstimes = 0;
  util::DateTime now(start);
  while (now <= t2) {
    ++nobstimes;
    now += freq;
  }
  const util::DateTime * bgn = &start;
  const util::Duration * stp = &freq;
  int iobs;
  qg_obsdb_generate_f90(keyHelp_, obsname.size(), obsname.c_str(), &configc,
                        &bgn, &stp, nobstimes, iobs);
  nobs = iobs;
}

// -----------------------------------------------------------------------------

int ObsHelpQG::nobs(const std::string & obsname) const {
  int iobs;
  qg_obsdb_nobs_f90(keyHelp_, obsname.size(), obsname.c_str(), iobs);
  return iobs;
}

// -----------------------------------------------------------------------------


}  // namespace qg
