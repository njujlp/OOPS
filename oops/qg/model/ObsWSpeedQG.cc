/*
 * (C) Copyright 2009-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "model/ObsWSpeedQG.h"

#include "util/Logger.h"
#include "model/GomQG.h"
#include "model/LocQG.h"
#include "model/ObsBias.h"
#include "model/ObsBiasIncrement.h"
#include "model/ObsSpaceQG.h"
#include "model/ObsVecQG.h"
#include "model/QgFortran.h"
#include "model/VarQG.h"
#include "eckit/config/Configuration.h"


using oops::Log;

// -----------------------------------------------------------------------------
namespace qg {
// -----------------------------------------------------------------------------

ObsWSpeedQG::ObsWSpeedQG(ObsSpaceQG & odb, const eckit::Configuration & config)
  : obsdb_(odb), obsname_("WSpeed"), varin_()
{
  const eckit::Configuration * configc = &config;
  qg_wspeed_setup_f90(keyOperWspeed_, &configc);
  int keyVarin;
  qg_obsoper_inputs_f90(keyOperWspeed_, keyVarin);
  varin_.reset(new VarQG(keyVarin));
  Log::trace() << "ObsWSpeedQG created " << obsname_ << std::endl;
}

// -----------------------------------------------------------------------------

ObsWSpeedQG::~ObsWSpeedQG() {
  qg_wspeed_delete_f90(keyOperWspeed_);
  Log::trace() << "ObsWSpeedQG destructed" << std::endl;
}

// -----------------------------------------------------------------------------

void ObsWSpeedQG::obsEquiv(const GomQG & gom, ObsVecQG & ovec,
                           const ObsBias & bias) const {
  Log::debug() << "ObsWSpeedQG obsEquiv gom : " << gom << std::endl;
  qg_wspeed_eqv_f90(gom.toFortran(), ovec.toFortran(), bias.wspd());
  Log::debug() << "ObsWSpeedQG obsEquiv ovec : " << ovec << std::endl;
}

// -----------------------------------------------------------------------------

void ObsWSpeedQG::generateObsError(const eckit::Configuration & conf) {
  const double err = conf.getDouble("obs_error");
  qg_obsdb_seterr_f90(obsdb_.toFortran(), keyOperWspeed_, err);
}

// -----------------------------------------------------------------------------

void ObsWSpeedQG::print(std::ostream & os) const {
  os << "ObsWSpeedQG::print not implemented";
}

// -----------------------------------------------------------------------------

}  // namespace qg
