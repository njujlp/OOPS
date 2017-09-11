/*
 * (C) Copyright 2009-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef OOPS_ASSIMILATION_COSTFCTWEAK_H_
#define OOPS_ASSIMILATION_COSTFCTWEAK_H_

#include <map>

#include "eckit/config/LocalConfiguration.h"
#include "util/Logger.h"
#include "oops/assimilation/CostFunction.h"
#include "oops/assimilation/CostJbJq.h"
#include "oops/assimilation/CostJcDFI.h"
#include "oops/assimilation/CostJo.h"
#include "oops/assimilation/CostTermBase.h"
#include "oops/base/PostProcessor.h"
#include "oops/base/PostProcessorTL.h"
#include "oops/base/PostProcessorAD.h"
#include "oops/base/StateInfo.h"
#include "oops/interface/Geometry.h"
#include "oops/interface/Increment.h"
#include "oops/interface/Model.h"
#include "oops/interface/State.h"
#include "oops/interface/Variables.h"
#include "util/DateTime.h"
#include "util/Duration.h"

namespace oops {

/// Weak Constraint 4D-Var Cost Function
/*!
 * General weak constraint constraint 4D-Var cost function.
 */

// -----------------------------------------------------------------------------

template<typename MODEL> class CostFctWeak : public CostFunction<MODEL> {
  typedef Increment<MODEL>           Increment_;
  typedef ControlIncrement<MODEL>    CtrlInc_;
  typedef ControlVariable<MODEL>     CtrlVar_;
  typedef CostFunction<MODEL>        CostFct_;
  typedef Geometry<MODEL>            Geometry_;
  typedef State<MODEL>               State_;
  typedef Model<MODEL>               Model_;
  typedef Variables<MODEL>           Variables_;

 public:
  CostFctWeak(const eckit::Configuration &, const Geometry_ &, const Model_ &);
  ~CostFctWeak() {}

  void runTLM(const CtrlInc_ &, PostProcessorTL<Increment_> &,
              PostProcessor<Increment_>) const override;
  void runADJ(CtrlInc_ &, PostProcessorAD<Increment_> &,
              PostProcessor<Increment_>) const override;
  void zeroAD(CtrlInc_ &) const override;

  void runTLM(CtrlInc_ &, const bool idmodel = false) const;
  void runADJ(CtrlInc_ &, const bool idmodel = false) const;

 private:
  void runNL(const CtrlVar_ &, PostProcessor<State_> &) const override;
  void addIncr(CtrlVar_ &, const CtrlInc_ &, PostProcessor<Increment_> &) const override;

  CostJbJq<MODEL>     * newJb(const eckit::Configuration &, const Geometry_ &, const CtrlVar_ &) const override;
  CostJo<MODEL>       * newJo(const eckit::Configuration &) const override;
  CostTermBase<MODEL> * newJc(const eckit::Configuration &, const Geometry_ &) const override;

  util::Duration windowLength_;
  util::DateTime windowBegin_;
  util::DateTime windowEnd_;
  util::Duration windowSub_;
  int nsubwin_;
  bool tlforcing_;
  const Variables_ ctlvars_;
};

// =============================================================================

template<typename MODEL>
CostFctWeak<MODEL>::CostFctWeak(const eckit::Configuration & config,
                                const Geometry_ & resol, const Model_ & model)
  : CostFunction<MODEL>::CostFunction(resol, model),
    tlforcing_(false), ctlvars_(config)
{
  windowLength_ = util::Duration(config.getString("window_length"));
  windowBegin_ = util::DateTime(config.getString("window_begin"));
  windowEnd_ = windowBegin_ + windowLength_;
  windowSub_ = util::Duration(config.getString("window_sub"));

  nsubwin_ = windowLength_.toSeconds() / windowSub_.toSeconds();
  ASSERT(windowLength_.toSeconds() == windowSub_.toSeconds()*nsubwin_);

  if (config.has("tlforcing")) {
    if (config.getString("tlforcing") == "on") tlforcing_ = true;
  }

  this->setupTerms(config);
  Log::trace() << "CostFctWeak constructed" << std::endl;
}

// -----------------------------------------------------------------------------

template <typename MODEL>
CostJbJq<MODEL> * CostFctWeak<MODEL>::newJb(const eckit::Configuration & jbConf,
                                            const Geometry_ & resol,
                                            const CtrlVar_ & xb) const {
  return new CostJbJq<MODEL>(jbConf, resol, ctlvars_, windowSub_, xb.state(), tlforcing_);
}

// -----------------------------------------------------------------------------

template <typename MODEL>
CostJo<MODEL> * CostFctWeak<MODEL>::newJo(const eckit::Configuration & joConf) const {
  return new CostJo<MODEL>(joConf, windowBegin_, windowEnd_, util::Duration(0), true);
}

// -----------------------------------------------------------------------------

template <typename MODEL>
CostTermBase<MODEL> * CostFctWeak<MODEL>::newJc(const eckit::Configuration & jcConf,
                                                const Geometry_ & resol) const {
  const eckit::LocalConfiguration jcdfi(jcConf, "jcdfi");
  const util::DateTime vt(windowBegin_ + windowLength_/2);
  return new CostJcDFI<MODEL>(jcdfi, resol, vt, windowLength_);
}

// -----------------------------------------------------------------------------

template <typename MODEL>
void CostFctWeak<MODEL>::runNL(const CtrlVar_ & xx,
                               PostProcessor<State_> & post) const {
  for (int jsub = 0; jsub < nsubwin_; ++jsub) {
    util::DateTime bgn(windowBegin_ + jsub*windowSub_);
    util::DateTime end(bgn + windowSub_);

    State_ zz(xx.state()[jsub]);

    ASSERT(zz.validTime() == bgn);
    CostFct_::getModel().forecast(zz, xx.modVar(), windowSub_, post);
    ASSERT(zz.validTime() == end);
  }
}

// -----------------------------------------------------------------------------

template <typename MODEL>
void CostFctWeak<MODEL>::runTLM(const CtrlInc_ & dx,
                                PostProcessorTL<Increment_> & cost,
                                PostProcessor<Increment_> post) const {
  Increment_ dw(dx.state()[0], false);
  for (int jsub = dx.state().first(); jsub <= dx.state().last(); ++jsub) {
    util::DateTime bgn(windowBegin_ + jsub*windowSub_);
    util::DateTime end(bgn + windowSub_);

    ASSERT(dx.state()[jsub].validTime() == bgn);
    Increment_ dz(dx.state()[jsub]);
    if (tlforcing_ && jsub > 0) dz += dw;
    CostFct_::getTLM(jsub).forecastTL(dz, dx.modVar(), windowSub_, post, cost);
    dw = dz;
    ASSERT(dz.validTime() == end);
  }
}

// -----------------------------------------------------------------------------

template <typename MODEL>
void CostFctWeak<MODEL>::runTLM(CtrlInc_ & dx, const bool idmodel) const {
  PostProcessor<Increment_> post;
  PostProcessorTL<Increment_> cost;
  ASSERT(!tlforcing_);

  for (int jsub = dx.state().first(); jsub <= dx.state().last(); ++jsub) {
    util::DateTime bgn(windowBegin_ + jsub*windowSub_);
    util::DateTime end(bgn + windowSub_);
    ASSERT(dx.state()[jsub].validTime() == bgn);

    if (idmodel) {
      dx.state()[jsub].updateTime(windowSub_);
    } else {
      CostFct_::getTLM(jsub).forecastTL(dx.state()[jsub], dx.modVar(), windowSub_, post, cost);
    }

    ASSERT(dx.state()[jsub].validTime() == end);
  }

  dx.state().shift_forward();
}

// -----------------------------------------------------------------------------

template <typename MODEL>
void CostFctWeak<MODEL>::zeroAD(CtrlInc_ & dx) const {
  for (int jsub = dx.state().first(); jsub <= dx.state().last(); ++jsub) {
    util::DateTime end(windowBegin_ + (jsub+1)*windowSub_);
    dx.state()[jsub].zero(end);
  }
  dx.modVar().zero();
  dx.obsVar().zero();
}

// -----------------------------------------------------------------------------

template <typename MODEL>
void CostFctWeak<MODEL>::runADJ(CtrlInc_ & dx,
                                PostProcessorAD<Increment_> & cost,
                                PostProcessor<Increment_> post) const {
  for (int jsub = dx.state().last(); jsub >= dx.state().first(); --jsub) {
    util::DateTime bgn(windowBegin_ + jsub*windowSub_);
    util::DateTime end(bgn + windowSub_);

    ASSERT(dx.state()[jsub].validTime() == end);
    CostFct_::getTLM(jsub).forecastAD(dx.state()[jsub], dx.modVar(), windowSub_, post, cost);
    if (tlforcing_ && jsub > 0) dx.state()[jsub-1] += dx.state()[jsub];
    ASSERT(dx.state()[jsub].validTime() == bgn);
  }
}

// -----------------------------------------------------------------------------

template <typename MODEL>
void CostFctWeak<MODEL>::runADJ(CtrlInc_ & dx, const bool idmodel) const {
  PostProcessor<Increment_> post;
  PostProcessorAD<Increment_> cost;
  ASSERT(!tlforcing_);

  dx.state().shift_backward();

  for (int jsub = dx.state().last(); jsub >= dx.state().first(); --jsub) {
    util::DateTime bgn(windowBegin_ + jsub*windowSub_);
    util::DateTime end(bgn + windowSub_);
    ASSERT(dx.state()[jsub].validTime() == end);

    if (idmodel) {
      dx.state()[jsub].updateTime(-windowSub_);
    } else {
      CostFct_::getTLM(jsub).forecastAD(dx.state()[jsub], dx.modVar(), windowSub_, post, cost);
    }

    ASSERT(dx.state()[jsub].validTime() == bgn);
  }
}

// -----------------------------------------------------------------------------

template<typename MODEL>
void CostFctWeak<MODEL>::addIncr(CtrlVar_ & xx, const CtrlInc_ & dx,
                                 PostProcessor<Increment_> & post) const {
  if (tlforcing_) {
    Increment_ xi(dx.state()[0]);
    for (int jsub = 0; jsub < nsubwin_; ++jsub) {
      if (jsub > 0) xi += dx.state()[jsub];
      xx.state()[jsub] += xi;
      if (jsub < nsubwin_-1) {
        CostFct_::getTLM(jsub).forecastTL(xi, dx.modVar(), windowSub_, post);
      }
    }
  } else {
    for (int jsub = 0; jsub < nsubwin_; ++jsub) {
      xx.state()[jsub] += dx.state()[jsub];
    }
  }
}

// -----------------------------------------------------------------------------

}  // namespace oops

#endif  // OOPS_ASSIMILATION_COSTFCTWEAK_H_
