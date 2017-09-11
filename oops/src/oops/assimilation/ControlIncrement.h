/*
 * (C) Copyright 2009-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef OOPS_ASSIMILATION_CONTROLINCREMENT_H_
#define OOPS_ASSIMILATION_CONTROLINCREMENT_H_

#include <ostream>
#include <sstream>
#include <string>

#include "util/Logger.h"
#include "oops/assimilation/Increment4D.h"
#include "oops/interface/ModelAuxIncrement.h"
#include "oops/interface/ObsAuxIncrement.h"
#include "eckit/config/Configuration.h"
#include "util/ObjectCounter.h"
#include "util/Printable.h"
#include "util/dot_product.h"

namespace oops {

/// Control variable increment
/*!
 * The control variable acts as a container for the inputs of the variational
 * data assimilation cost functions in physical space.
 * That includes the states at the start the assimilation window or of each
 * sub-window but also additional variables such as model bias, VarBC
 * coefficients, or other control variables for algorithms that use them.
 * The control variable increment contains variations of the
 * control variable.
 */

template<typename MODEL> class CostJbTotal;

template<typename MODEL> class ControlIncrement;

// -----------------------------------------------------------------------------
template<typename MODEL>
class ControlIncrement : public util::Printable,
                         private util::ObjectCounter<ControlIncrement<MODEL> > {
  typedef CostJbTotal<MODEL>         JbTotal_;
  typedef Increment4D<MODEL>         Increment4D_;
  typedef ModelAuxIncrement<MODEL>   ModelAuxIncr_;
  typedef ObsAuxIncrement<MODEL>     ObsAuxIncr_;

 public:
  static const std::string classname() {return "oops::ControlIncrement";}

/// Constructor, destructor
  explicit ControlIncrement(const JbTotal_ &);
  ControlIncrement(const ControlIncrement &, const bool copy = true);
  ControlIncrement(const ControlIncrement &, const eckit::Configuration &);
  ~ControlIncrement();

/// Linear algebra operators
  void zero();
  ControlIncrement & operator=(const ControlIncrement &);
  ControlIncrement & operator+=(const ControlIncrement &);
  ControlIncrement & operator-=(const ControlIncrement &);
  ControlIncrement & operator*=(const double);
  void axpy(const double, const ControlIncrement &);
  double dot_product_with(const ControlIncrement &) const;

/// I/O and diagnostics
  void read(const eckit::Configuration &);
  void write(const eckit::Configuration &) const;

/// Get state control variable
  Increment4D_ & state() {return incrm4d_;}
  const Increment4D_ & state() const {return incrm4d_;}

/// Get augmented model control variable
  ModelAuxIncr_ & modVar() {return modbias_;}
  const ModelAuxIncr_ & modVar() const {return modbias_;}

/// Get augmented observation control variable
  ObsAuxIncr_ & obsVar() {return obsbias_;}
  const ObsAuxIncr_ & obsVar() const {return obsbias_;}

 private:
  void print(std::ostream &) const;
  Increment4D_  incrm4d_;
  ModelAuxIncr_ modbias_;   // not only for bias, better name?
  ObsAuxIncr_   obsbias_;   // not only for bias, better name?
};

// =============================================================================

template<typename MODEL>
ControlIncrement<MODEL>::ControlIncrement(const JbTotal_ & jb)
  : incrm4d_(jb.jbState()), modbias_(jb.resolution(), jb.jbModBias().config()),
    obsbias_(jb.jbObsBias().config())
{
  Log::trace() << "ControlIncrement:ControlIncrement created." << std::endl;
}
// -----------------------------------------------------------------------------
template<typename MODEL>
ControlIncrement<MODEL>::ControlIncrement(const ControlIncrement & other, const bool copy)
  : incrm4d_(other.incrm4d_, copy), modbias_(other.modbias_, copy),
    obsbias_(other.obsbias_, copy)
{
  Log::trace() << "ControlIncrement:ControlIncrement copied." << std::endl;
}
// -----------------------------------------------------------------------------
template<typename MODEL>
ControlIncrement<MODEL>::ControlIncrement(const ControlIncrement & other,
                                          const eckit::Configuration & tlConf)
  : incrm4d_(other.incrm4d_, tlConf), modbias_(other.modbias_, tlConf),
    obsbias_(other.obsbias_, tlConf)
{
  Log::trace() << "ControlIncrement:ControlIncrement copied." << std::endl;
}
// -----------------------------------------------------------------------------
template<typename MODEL>
ControlIncrement<MODEL>::~ControlIncrement() {}
// -----------------------------------------------------------------------------
template<typename MODEL> ControlIncrement<MODEL> &
ControlIncrement<MODEL>::operator=(const ControlIncrement & rhs) {
  incrm4d_ = rhs.incrm4d_;
  modbias_ = rhs.modbias_;
  obsbias_ = rhs.obsbias_;
  return *this;
}
// -----------------------------------------------------------------------------
template<typename MODEL> ControlIncrement<MODEL> &
ControlIncrement<MODEL>::operator+=(const ControlIncrement & rhs) {
  incrm4d_ += rhs.incrm4d_;
  modbias_ += rhs.modbias_;
  obsbias_ += rhs.obsbias_;
  return *this;
}
// -----------------------------------------------------------------------------
template<typename MODEL> ControlIncrement<MODEL> &
ControlIncrement<MODEL>::operator-=(const ControlIncrement & rhs) {
  incrm4d_ -= rhs.incrm4d_;
  modbias_ -= rhs.modbias_;
  obsbias_ -= rhs.obsbias_;
  return *this;
}
// -----------------------------------------------------------------------------
template<typename MODEL>
ControlIncrement<MODEL> & ControlIncrement<MODEL>::operator*=(const double zz) {
  incrm4d_ *= zz;
  modbias_ *= zz;
  obsbias_ *= zz;
  return *this;
}
// -----------------------------------------------------------------------------
template<typename MODEL>
void ControlIncrement<MODEL>::zero() {
  incrm4d_.zero();
  modbias_.zero();
  obsbias_.zero();
}
// -----------------------------------------------------------------------------
template<typename MODEL>
void ControlIncrement<MODEL>::axpy(const double zz, const ControlIncrement & rhs) {
  incrm4d_.axpy(zz, rhs.incrm4d_);
  modbias_.axpy(zz, rhs.modbias_);
  obsbias_.axpy(zz, rhs.obsbias_);
}
// -----------------------------------------------------------------------------
template<typename MODEL>
void ControlIncrement<MODEL>::read(const eckit::Configuration & config) {
  incrm4d_.read(config);
  modbias_.read(config);
  obsbias_.read(config);
}
// -----------------------------------------------------------------------------
template<typename MODEL>
void ControlIncrement<MODEL>::write(const eckit::Configuration & config) const {
  incrm4d_.write(config);
  modbias_.write(config);
  obsbias_.write(config);
}
// -----------------------------------------------------------------------------
template <typename MODEL>
void ControlIncrement<MODEL>::print(std::ostream & outs) const {
  outs << incrm4d_;
  outs << modbias_;
  outs << obsbias_;
}
// -----------------------------------------------------------------------------
template<typename MODEL>
double ControlIncrement<MODEL>::dot_product_with(const ControlIncrement & x2) const {
  double zz = 0.0;
  zz += dot_product(incrm4d_, x2.incrm4d_);
  zz += dot_product(modbias_, x2.modbias_);
  zz += dot_product(obsbias_, x2.obsbias_);
  return zz;
}
// -----------------------------------------------------------------------------
}  // namespace oops

#endif  // OOPS_ASSIMILATION_CONTROLINCREMENT_H_
