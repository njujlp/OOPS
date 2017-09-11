/*
 * (C) Copyright 1996-2017 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#ifndef eckit_cmd_LockCmd_H
#define eckit_cmd_LockCmd_H

#include "eckit/cmd/CmdResource.h"

//-----------------------------------------------------------------------------

namespace eckit {

//-----------------------------------------------------------------------------

class LockCmd : public CmdResource {
public:
    // -- Contructors

    LockCmd();

    // -- Destructor

    ~LockCmd();

private:
    // No copy allowed

    LockCmd(const LockCmd&);
    LockCmd& operator=(const LockCmd&);

    // -- Overridden methods

    // From CmdResource

    virtual void execute(std::istream&, std::ostream&, CmdArg&);
    virtual void man(std::ostream&) const;
    virtual void help(std::ostream&) const;
    virtual Arg usage(const std::string& cmd) const;
};

inline void destroy(LockCmd**) {
}

//-----------------------------------------------------------------------------

} // namespace eckit

#endif  // LockCmd_H
