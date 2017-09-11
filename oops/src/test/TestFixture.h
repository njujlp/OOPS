/*
 * (C) Copyright 1996-2017 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#pragma once

#include "eckit/testing/Setup.h"
#include "util/LibOOPS.h"

namespace test {

struct TestFixture : public eckit::testing::Setup {

    TestFixture() : eckit::testing::Setup() {
        // Common setup for every boost unit-test goes here
    }

    ~TestFixture() {
        oops::LibOOPS::instance().finalise();
    }
};

//----------------------------------------------------------------------------------------------------------------------

} // end namespace test
