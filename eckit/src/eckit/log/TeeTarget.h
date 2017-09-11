/*
 * (C) Copyright 1996-2017 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @file TeeTarget.h
/// @author Baudouin Raoult
/// @author Tiago Quintino
/// @date   August 2016

#ifndef eckit_log_TeeTarget_h
#define eckit_log_TeeTarget_h


#include "eckit/log/LogTarget.h"

namespace eckit {

//----------------------------------------------------------------------------------------------------------------------

class TeeTarget : public LogTarget {

public: // methods

    TeeTarget(LogTarget* left, LogTarget* right);

    virtual ~TeeTarget();

protected:
    void print(std::ostream& s) const;

private:
    LogTarget* left_;
    LogTarget* right_;

private:

    virtual void write(const char* start, const char* end);
    virtual void flush();

};

//----------------------------------------------------------------------------------------------------------------------

} // namespace eckit

#endif
