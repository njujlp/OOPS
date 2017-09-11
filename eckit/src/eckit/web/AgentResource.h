/*
 * (C) Copyright 1996-2017 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

// File AgentResource.h
// Baudouin Raoult - ECMWF Sep 97

#ifndef AgentResource_H
#define AgentResource_H

#include "eckit/web/HtmlResource.h"

//-----------------------------------------------------------------------------

namespace eckit {

//-----------------------------------------------------------------------------

class AgentResource : public HtmlResource {
public:


	AgentResource();

	virtual ~AgentResource();

protected: // members

	std::string name_;


protected: // overridden methods

	virtual void html(std::ostream&,Url&);

};

//-----------------------------------------------------------------------------

} // namespace eckit

#endif
