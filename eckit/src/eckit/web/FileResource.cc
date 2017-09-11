/*
 * (C) Copyright 1996-2017 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "eckit/filesystem/PathName.h"
#include "eckit/io/StdFile.h"
#include "eckit/web/FileResource.h"
#include "eckit/web/HttpBuf.h"
#include "eckit/web/Url.h"

//-----------------------------------------------------------------------------

namespace eckit {

//-----------------------------------------------------------------------------

FileResource::FileResource():
	HtmlResource("/html")
{
}

FileResource::~FileResource()
{
}

void FileResource::html(std::ostream& s,Url& url)
{
	eckit::PathName path("~/http/" + url.name());

	StdFile file(path,"r");
	char line[1024];

	s << HttpBuf::dontEncode;

	while(fgets(line,sizeof(line),file))
		s << line;

	s << HttpBuf::doEncode;
}

static FileResource fileResourceInstance;

//-----------------------------------------------------------------------------

} // namespace eckit

