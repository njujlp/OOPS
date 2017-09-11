/*
 * (C) Copyright 1996-2017 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

// File HttpHeader.h
// Manuel Fuentes - ECMWF Oct 96

#ifndef HttpHeader_H
#define HttpHeader_H

#include "eckit/eckit.h"

#include "eckit/memory/NonCopyable.h"
#include "eckit/io/ResizableBuffer.h"

//-----------------------------------------------------------------------------

namespace eckit {

//-----------------------------------------------------------------------------

class HttpHeader : private eckit::NonCopyable {

public: // methods

	HttpHeader();

    ~HttpHeader();

    HttpHeader& operator=(std::map<std::string,std::string,std::less<std::string> >&);

	void length(const long);
	long contentLength() const;
	void type(const std::string& );
	void status(const long);
	void authenticate(const std::string& );
	bool authenticated() const;
	void forward(const std::string& );
	void dontCache();

    const std::string& type() const;

	const std::string& getHeader(const std::string&) const;
	void setHeader(const std::string&,const std::string&);

	void  content(const char*,long);
    const char* content() const { return content_; }

protected: // methods
	
    void print(std::ostream&) const;

private: // members

	std::string version_;
	long   statusCode_;
	long   contentLength_;

	struct compare {
		bool operator()(const std::string&,const std::string&) const;
	};

	typedef std::map<std::string,std::string,HttpHeader::compare> Map;

	Map header_;
    eckit::ResizableBuffer content_;

private: // methods

	friend std::ostream& operator<<(std::ostream& s,const HttpHeader& p)
		{ p.print(s); return s; }

};

//-----------------------------------------------------------------------------

} // namespace eckit


#endif
