/*
 * (C) Copyright 2009-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef QG_MODEL_QGTRAITS_H_
#define QG_MODEL_QGTRAITS_H_

#include <string>

#include "model/GomQG.h"
#include "model/LinearObsOp.h"
#include "model/LocQG.h"
#include "model/ModelBias.h"
#include "model/ModelBiasIncrement.h"
#include "model/ModelBiasCovariance.h"
#include "model/ObsBias.h"
#include "model/ObsBiasIncrement.h"
#include "model/ObsBiasCovariance.h"
#include "model/ObsSpaceQG.h"
#include "model/ObsVecQG.h"
#include "model/QgErrorCovariance.h"
#include "model/QgGeometry.h"
#include "model/QgIncrement.h"
#include "model/QgModel.h"
#include "model/QgObservation.h"
#include "model/QgState.h"
#include "model/VarQG.h"

namespace qg {

struct QgTraits {
  static std::string name() {return "QG";}
  static std::string nameCovar() {return "QgError";}

  typedef qg::QgGeometry            Geometry;
  typedef qg::VarQG                 Variables;

  typedef qg::QgState               State;
  typedef qg::QgModel               Model;
  typedef qg::QgIncrement           Increment;
  typedef qg::QgErrorCovariance     Covariance;

  typedef qg::ModelBias             ModelAuxControl;
  typedef qg::ModelBiasIncrement    ModelAuxIncrement;
  typedef qg::ModelBiasCovariance   ModelAuxCovariance;

  typedef qg::ObsSpaceQG            ObsSpace;
  typedef qg::QgObservation         ObsOperator;
  typedef qg::LinearObsOp           LinearObsOperator;
  typedef qg::ObsVecQG              ObsVector;

  typedef qg::ObsBias               ObsAuxControl;
  typedef qg::ObsBiasIncrement      ObsAuxIncrement;
  typedef qg::ObsBiasCovariance     ObsAuxCovariance;

  typedef qg::GomQG                 ModelAtLocations;
  typedef qg::LocQG                 Locations;
};

}  // namespace qg

#endif  // QG_MODEL_QGTRAITS_H_
