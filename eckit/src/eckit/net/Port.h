/*
 * (C) Copyright 1996-2017 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @file   Port.h
/// @author Baudouin Raoult
/// @date   Jul 96

#ifndef eckit_Port_h
#define eckit_Port_h

#include "eckit/eckit.h"

namespace eckit {

//----------------------------------------------------------------------------------------------------------------------

class Port {
public:

// -- Contructors

	Port(const std::string&, int );

// -- Operators

    operator int() const { return port_; }

private:

// There is no private copy constructor as this will confuse g++ 4.x.x

// -- Members

	int   port_;
};

//----------------------------------------------------------------------------------------------------------------------

} // namespace eckit

#endif
