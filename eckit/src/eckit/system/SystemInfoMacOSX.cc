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
/// @date   May 2016

#include <sys/param.h>
#include <mach-o/dyld.h>
#include <mach/mach.h>
#include <malloc/malloc.h>

#include "eckit/system/SystemInfoMacOSX.h"

#include "eckit/io/Buffer.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/LocalPathName.h"

namespace eckit {
namespace system {

//----------------------------------------------------------------------------------------------------------------------

SystemInfoMacOSX::~SystemInfoMacOSX() {
}

LocalPathName SystemInfoMacOSX::executablePath() const
{
    Buffer buffer(MAXPATHLEN);

    int err = 0;
    uint32_t actual = uint32_t(buffer.size());
    if( (err = _NSGetExecutablePath(buffer, &actual)) == -1 ) {
        buffer.resize(actual);
        err = _NSGetExecutablePath(buffer, &actual);
    }

    if(err != 0) {
        std::ostringstream oss;
        oss << "_NSGetExecutablePath when called with buffer sized " << buffer.size();
        throw FailedSystemCall(oss.str(), Here());
    }

    std::string path(buffer);

    return LocalPathName(path).realName();
}

Mem SystemInfoMacOSX::memoryUsage() const {
    struct task_basic_info info;
    mach_msg_type_number_t size = sizeof(info);

    kern_return_t err = task_info(mach_task_self(),
                                  TASK_BASIC_INFO,
                                  (task_info_t)&info,
                                  &size);

    if ( err != KERN_SUCCESS ) {
        throw eckit::FailedSystemCall(mach_error_string(err), Here());
    }

    return Mem(info.resident_size, info.virtual_size);}

size_t SystemInfoMacOSX::memoryAllocated() const {
    return mstats().bytes_used;
}

//----------------------------------------------------------------------------------------------------------------------

} // namespace system
} // namespace eckit

