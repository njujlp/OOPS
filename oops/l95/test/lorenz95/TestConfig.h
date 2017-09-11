/*
 * (C) Copyright 2009-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef _LORENZ95_TESTCONFIG_H_
#define _LORENZ95_TESTCONFIG_H_

#include <string>

#include <boost/scoped_ptr.hpp>
#include <boost/test/unit_test.hpp>

#include "eckit/runtime/Main.h"
#include "eckit/config/YAMLConfiguration.h"
#include "eckit/config/LocalConfiguration.h"
#include "eckit/exception/Exceptions.h"

#include "util/Logger.h"

using oops::Log;

namespace test {

// -----------------------------------------------------------------------------

class TestConfig {

public:

  static const eckit::Configuration & config() {
    static TestConfig test;
    return *test.config_;
  }

  TestConfig() {

    ASSERT( eckit::Main::ready() );

    int narg = eckit::Main::instance().argc();
    ASSERT(narg >= 2);
    eckit::PathName fname = eckit::Main::instance().argv(narg-1);
    config_.reset(new eckit::YAMLConfiguration(fname));
  }

  ~TestConfig() {}

private:
  boost::scoped_ptr<const eckit::YAMLConfiguration> config_;
};

// -----------------------------------------------------------------------------

}  // namespace test

#endif  // _LORENZ95_TESTCONFIG_H_
