/*
 * (C) Copyright 2009-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "model/ObsWindQG.h"

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

ObsWindQG::ObsWindQG(ObsSpaceQG & odb, const eckit::Configuration & config)
  : obsdb_(odb), obsname_("Wind"), varin_()
{
  const eckit::Configuration * configc = &config;
  qg_wind_setup_f90(keyOperWind_, &configc);
  int keyVarin;
  qg_obsoper_inputs_f90(keyOperWind_, keyVarin);
  varin_.reset(new VarQG(keyVarin));
  Log::trace() << "ObsWindQG created " << obsname_ << std::endl;
}

// -----------------------------------------------------------------------------

ObsWindQG::~ObsWindQG() {
  qg_wind_delete_f90(keyOperWind_);
}

// -----------------------------------------------------------------------------

void ObsWindQG::obsEquiv(const GomQG & gom, ObsVecQG & ovec,
                         const ObsBias & bias) const {
  qg_wind_equiv_f90(gom.toFortran(), ovec.toFortran(), bias.wind());
}

// -----------------------------------------------------------------------------

void ObsWindQG::generateObsError(const eckit::Configuration & conf) {
  const double err = conf.getDouble("obs_error");
  qg_obsdb_seterr_f90(obsdb_.toFortran(), keyOperWind_, err);
}

// -----------------------------------------------------------------------------

void ObsWindQG::print(std::ostream & os) const {
  os << "ObsWindQG::print not implemented";
}

// -----------------------------------------------------------------------------

}  // namespace qg
