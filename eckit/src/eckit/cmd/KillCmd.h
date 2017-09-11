/*
 * (C) Copyright 1996-2017 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @file   KillCmd.h
/// @author Baudouin Raoult
/// @date   Jan 1997

#ifndef eckit_cmd_KillCmd_H
#define eckit_cmd_KillCmd_H

#include "eckit/cmd/CmdResource.h"

//-----------------------------------------------------------------------------

namespace eckit {

//-----------------------------------------------------------------------------

class KillCmd : public CmdResource {
public:
    // -- Contructors

    KillCmd();

    // -- Destructor

    ~KillCmd();

private:
    // No copy allowed

    KillCmd(const KillCmd&);
    KillCmd& operator=(const KillCmd&);

    // -- Methods

    void kill(pid_t, std::ostream&) const;

    // -- Overridden methods

    // From CmdResource

    virtual void execute(std::istream&, std::ostream&, CmdArg&);

    virtual void help(std::ostream&) const;
    virtual Arg usage(const std::string& cmd) const;
};

//-----------------------------------------------------------------------------

} // namespace eckit

#endif
