/*
 * (C) Copyright 1996-2017 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Baudouin Raoult
/// @author Tiago Quintino
/// @date   August 2016

#include <stdlib.h>

#include <algorithm>
#include <string>

#include "eckit/config/LibEcKit.h"

#include "eckit/eckit_version.h"
#include "eckit/thread/AutoLock.h"

namespace eckit {

//----------------------------------------------------------------------------------------------------------------------

REGISTER_LIBRARY(LibEcKit);

LibEcKit::LibEcKit() :
    Library("eckit"),
    abort_handler_(&(::abort))
{
}

LibEcKit& LibEcKit::instance()
{
    static LibEcKit libeckit;
    return libeckit;
}

void LibEcKit::setAbortHandler(abort_handler_t h)
{
    AutoLock<LibEcKit> lock(*this);
    if(h) { abort_handler_ = h; }
}

void LibEcKit::abort()
{
    abort_handler_();
}

const void* LibEcKit::addr() const { return this; }

std::string LibEcKit::version() const { return eckit_version_str(); }

std::string LibEcKit::gitsha1(unsigned int count) const {
    std::string sha1(eckit_git_sha1());
    if(sha1.empty()) {
        return "not available";
    }

    return sha1.substr(0,std::min(count,40u));
}

//----------------------------------------------------------------------------------------------------------------------

} // namespace eckit

