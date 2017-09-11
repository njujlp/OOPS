/*
 * (C) Copyright 1996-2017 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @file PipeApplication.h
/// @author Baudouin Raoult
/// @author Florian Rathgeber

#ifndef eckit_PipeApplication_h
#define eckit_PipeApplication_h

#include "eckit/config/Resource.h"
#include "eckit/runtime/Application.h"

//-----------------------------------------------------------------------------

namespace eckit {

class Stream;

//-----------------------------------------------------------------------------

class PipeApplication : public Application {

public: // methods

    PipeApplication(int argc, char **argv, const char* homeenv);

    virtual ~PipeApplication();

    virtual void process(Stream&) = 0;
    virtual void endBatch();
    virtual void init(Stream&);
    virtual void waiting();

    static void launch(const std::string& name, int in, int out);

private: // members

    Resource<long> in_;
    Resource<long> out_;

    /// overridden from Application
    virtual void run();
};

//-----------------------------------------------------------------------------

} // namespace eckit

#endif
