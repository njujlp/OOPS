/*
 * (C) Copyright 1996-2017 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

// File filesystem/BasePathName.h
// Baudouin Raoult - (c) ECMWF Jun 11

#ifndef eckit_filesystem_BasePathName_h
#define eckit_filesystem_BasePathName_h

#include "eckit/eckit.h"

#include "eckit/io/DataHandle.h"
#include "eckit/io/Length.h"
#include "eckit/memory/NonCopyable.h"

struct FileSystemSize;

//-----------------------------------------------------------------------------

namespace eckit {

//-----------------------------------------------------------------------------

class BasePathName : protected NonCopyable {
public:

// -- Contructors

	BasePathName() {}

// -- Destructor

	virtual ~BasePathName() {}

// -- Methods

    virtual BasePathName* clone() const = 0;
    virtual const char* localPath() const = 0;
    virtual Length size() const = 0;
    virtual time_t lastAccess() const = 0;
    virtual time_t lastModified() const = 0;
    virtual time_t created() const = 0;

    virtual bool isDir() const = 0;
    virtual bool isLink() const = 0;

    virtual void rename(const BasePathName&) const = 0;
    virtual void link(const BasePathName&) const = 0;
    virtual bool sameAs(const BasePathName&) const = 0;
    virtual BasePathName* mountPoint() const = 0;
    virtual BasePathName* realName() const = 0;

    virtual bool exists() const = 0;
    virtual bool available() const = 0;
    virtual void mkdir(short) const = 0;
    virtual void unlink() const = 0;
    virtual void rmdir() const = 0;
    virtual void touch() const = 0;
    virtual void children(std::vector<BasePathName*>&,std::vector<BasePathName*>&) const = 0;
    virtual void match(std::vector<BasePathName*>&,bool) const = 0;
    virtual void reserve(const Length&) const = 0;

    virtual BasePathName* unique() const = 0;
    virtual BasePathName* dirName() const = 0;
    virtual BasePathName* fullName() const = 0;
    virtual BasePathName* orphanName() const = 0;
    virtual BasePathName* checkClusterNode() const = 0;
    virtual BasePathName* baseName(bool) const = 0;
    virtual std::string extension() const = 0;

    virtual std::string clusterName() const = 0;

    virtual void syncParentDirectory() const = 0;

    virtual std::string asString() const = 0;
    virtual void fileSystemSize(FileSystemSize&) const = 0;
    virtual DataHandle* fileHandle(bool overwrite) const = 0;
    virtual DataHandle* partHandle(const OffsetList&, const LengthList&) const = 0;
    virtual DataHandle* partHandle(const Offset&, const Length&) const = 0;

    virtual const std::string& node() const = 0;
    virtual const std::string& path() const = 0;

protected:

    virtual void print(std::ostream&) const = 0;

private:

	friend std::ostream& operator<<(std::ostream& s,const BasePathName& p)
		{ p.print(s); return s; }

};

//-----------------------------------------------------------------------------

} // namespace eckit

#endif
