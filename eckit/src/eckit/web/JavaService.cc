/*
 * (C) Copyright 1996-2017 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


#include "eckit/web/JavaService.h"
#include "eckit/web/JavaUser.h"

//-----------------------------------------------------------------------------

namespace eckit {

//-----------------------------------------------------------------------------


JavaService::JavaService(int port):
	NetService(port)
{
}

JavaService::~JavaService()
{
}

NetUser* JavaService::newUser(TCPSocket& socket)
{ 
	return new JavaUser(socket); 
}

//-----------------------------------------------------------------------------

} // namespace eckit

