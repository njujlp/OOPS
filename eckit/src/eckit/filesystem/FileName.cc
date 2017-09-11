/*
 * (C) Copyright 1996-2017 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


#include "eckit/filesystem/FileName.h"
#include "eckit/parser/Tokenizer.h"
#include "eckit/filesystem/FileManager.h"

//-----------------------------------------------------------------------------

namespace eckit {

//-----------------------------------------------------------------------------

FileName::FileName(const std::string& path)
{
    Tokenizer parse(":");
    std::vector<std::string> s;

    parse(path,s);

    switch (s.size()) {
    case 1:
        scheme_ = "unix";
        name_   = s[0];
        break;

    case 2:
        name_   = s[1];
        scheme_ = s[0];
        break;

    default:
        scheme_ = s[0];
        name_   = s[1];
        for (size_t j = 2; j < s.size() ; j++)
            name_ = name_ + ':' + s[j];
        break;
    }
}

FileName::~FileName()
{
}

bool FileName::exists() const
{
    return FileManager::lookUp(scheme_).exists(*this);
}

DataHandle*  FileName::newWriteHandle() const
{
    return FileManager::lookUp(scheme_).newWriteHandle(*this);
}

DataHandle*  FileName::newReadHandle(const OffsetList& ol, const LengthList& ll) const
{
    return FileManager::lookUp(scheme_).newReadHandle(*this, ol, ll);
}

DataHandle*  FileName::newReadHandle() const
{
    return FileManager::lookUp(scheme_).newReadHandle(*this);
}

void FileName::print(std::ostream& s) const 
{
    s << "FileName[scheme=" << scheme_ << ",name=" << name_ << "]";
}

//-----------------------------------------------------------------------------

} // namespace eckit
